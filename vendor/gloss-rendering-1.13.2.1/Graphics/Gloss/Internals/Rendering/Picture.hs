{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_HADDOCK hide #-}

module Graphics.Gloss.Internals.Rendering.Picture
        (renderPicture)
where
import Graphics.Gloss.Internals.Rendering.State
import Graphics.Gloss.Internals.Rendering.Common
import Graphics.Gloss.Internals.Rendering.Circle
import Graphics.Gloss.Internals.Rendering.Polygon
import Graphics.Gloss.Internals.Rendering.Bitmap
import Graphics.Gloss.Internals.Data.Picture
import Graphics.Gloss.Internals.Data.Color
import System.Mem.StableName
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc                            (free)
import Foreign.Marshal.Array                            (mallocArray)
import Foreign.Ptr                                      (plusPtr)
import Foreign.Storable                                 (pokeElemOff)
import Data.IORef
import Data.List
import Control.Monad
import Graphics.Rendering.OpenGL                        (($=), get)
import qualified Graphics.Rendering.OpenGL.GL           as GL
import qualified Graphics.Rendering.OpenGL.GLU.Errors   as GLU
import qualified Graphics.UI.GLUT                       as GLUT


-- [Note: Batching, patched for Borrowed Light]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Gloss used to draw every convex polygon on its own: glBegin GL_POLYGON, one
-- glVertex call per corner, glEnd, with glColor4f before and after every Color
-- node. The game draws about four thousand polygons a frame, and those calls,
-- not the drawing itself, set its frame rate.
--
-- Now a convex polygon is not drawn on the spot. It is cut into the triangle fan
-- (p0 p1 p2) (p0 p2 p3) ... (p0 p[n-2] p[n-1]) and written, each corner with the
-- colour it is drawn in, into a buffer (the 'Batch'). Whatever is waiting there is
-- drawn with one glDrawArrays GL_TRIANGLES as soon as anything else has to happen
-- in OpenGL: a line, circle, text, bitmap or non-convex polygon is drawn (those
-- still go straight to OpenGL), a transform pushes or pops the matrix, the buffer
-- is full, or the frame ends. The image does not change, for these reasons:
--
--   * Same shapes. OpenGL draws a convex GL_POLYGON as exactly the region the
--     fan of triangles from its first corner covers, and the rasteriser's rules
--     for shared edges give every pixel of that region to exactly one triangle:
--     no pixel is drawn twice (which would show through alpha blending) and none
--     is missed. (Drivers commonly draw GL_POLYGON as this very fan themselves;
--     screenshots of the game before and after this change match pixel for
--     pixel.)
--
--   * Same colours. Each corner carries the colour Gloss would have made current
--     with glColor4f for that polygon, the same four floats, so each vertex
--     reaches OpenGL with the same colour as before.
--
--   * Same order. Triangles in one glDrawArrays are drawn in the order they are
--     in the buffer, which is the order of the picture, and the batch is drawn
--     before anything that is not in it. Blending sees the same sequence.
--
--   * Same transform. A batch is drawn before every push and pop of the matrix,
--     so all its triangles are drawn under the matrix they were meant for.
--
-- The colour OpenGL has current is now set only when something drawn straight
-- to OpenGL needs it (see 'useColour'), and after a glDrawArrays with a colour
-- array it is unknown (OpenGL leaves it undefined), so it is set again before
-- the next such drawing. At the end of the frame it is put back to what it was
-- at the start, as Gloss's own restores used to leave it.


-- | Render a picture into the current OpenGL context.
--
--   Assumes that the OpenGL matrix mode is set to @Modelview@
--
renderPicture
        :: State        -- ^ Current rendering state.
        -> Float        -- ^ View port scale, which controls the level of detail.
                        --   Use 1.0 to start with.
        -> Picture      -- ^ Picture to render.
        -> IO ()

renderPicture state circScale picture
 = do
        -- Setup render state for world
        setLineSmooth   (stateLineSmooth state)
        setBlendAlpha   (stateBlendAlpha state)

        -- Draw the picture
        checkErrors "before drawPicture."
        -- Patched for Borrowed Light: the current colour is asked of OpenGL once per
        -- frame, here, and then carried down the picture as an argument. Gloss used
        -- to ask for it again at every Color node (and every matrix push asked for
        -- the matrix mode), and each of those queries can make the driver finish the
        -- work queued so far before it answers. With a few thousand of them in a
        -- frame, the queries, not the drawing, set the frame rate.
        initial <- get GL.currentColor

        -- Patched for Borrowed Light: the batch of convex polygons (see [Note:
        -- Batching]) is read from two client-side arrays, positions and colours,
        -- interleaved in one buffer. They are switched on for the frame and off at
        -- its end. Drawing with glBegin/glEnd, as everything outside the batch
        -- still does, does not read them.
        glColour <- newIORef (Just initial)
        let ctx = Ctx state glColour
        GL.clientState GL.VertexArray $= GL.Enabled
        GL.clientState GL.ColorArray  $= GL.Enabled

        drawPicture ctx circScale initial picture

        -- Draw what is still waiting, then leave OpenGL as Gloss used to: no
        -- arrays switched on, and the colour that was current at the start.
        flushBatch ctx
        GL.clientState GL.VertexArray $= GL.Disabled
        GL.clientState GL.ColorArray  $= GL.Disabled
        known   <- readIORef glColour
        when (known /= Just initial)
         $ GL.currentColor $= initial

        -- Forget the tessellations of polygons not drawn this frame.
        ageTessCache (stateTessCache state)

        checkErrors "after drawPicture."


-- | Patched for Borrowed Light: what drawing a frame needs besides the picture.
data Ctx
        = Ctx
        { -- | The render state, which holds the batch and the tessellation cache.
          ctxState      :: !State

          -- | The colour OpenGL has current, when it is known: 'Nothing' after a
          --   batch was drawn, since a colour array leaves it undefined.
        , ctxGLColour   :: !(IORef (Maybe (GL.Color4 GL.GLfloat))) }


-- | Draw one picture. @colour@ is the colour it is drawn in: the colour of the
--   innermost 'Color' node above it, or the frame's starting colour.
--
--   Patched for Borrowed Light: the colour is no longer made current in OpenGL
--   at every 'Color' node (and restored after it). Convex polygons carry it into
--   the batch, and everything drawn straight to OpenGL calls 'useColour' first,
--   which sets it only when OpenGL's current colour is different.
--   Every branch leaves the matrix as it found it.
drawPicture :: Ctx -> Float -> GL.Color4 GL.GLfloat -> Picture -> IO ()
drawPicture ctx circScale colour picture
 = {-# SCC "drawComponent" #-}
   let state = ctxState ctx
   in case picture of

        -- nothin'
        Blank
         ->     return ()

        -- line
        Line path
         -> do  direct ctx colour
                GL.renderPrimitive GL.LineStrip
                 $ vertexPFs path

        -- polygon (where?)
        Polygon path
         | stateWireframe state
         -> do  direct ctx colour
                GL.renderPrimitive GL.LineLoop
                 $ vertexPFs path

         -- Patched for Borrowed Light: a convex polygon goes into the batch, where
         -- it used to be drawn on its own as GL_POLYGON. See [Note: Batching].
         | isConvex path
         -> addFan ctx colour path

         -- Patched for Borrowed Light: a polygon that is not convex is drawn as
         -- before, as GLU's triangles, but the triangles come from the cache when
         -- the same polygon was drawn in this frame or the last one.
         | otherwise
         -> do  triangles <- tessellation (stateTessCache state) path
                direct ctx colour
                GL.renderPrimitive GL.Triangles
                 $ mapM_ GL.vertex triangles

        -- circle
        Circle radius
         ->  direct ctx colour >> renderCircle 0 0 circScale radius 0

        ThickCircle radius thickness
         ->  direct ctx colour >> renderCircle 0 0 circScale radius thickness

        -- arc
        Arc a1 a2 radius
         ->  direct ctx colour >> renderArc 0 0 circScale radius a1 a2 0

        ThickArc a1 a2 radius thickness
         ->  direct ctx colour >> renderArc 0 0 circScale radius a1 a2 thickness

        -- stroke text
        --      text looks weird when we've got blend on,
        --      so disable it during the renderString call.
        Text str
         -> do
                direct ctx colour
                GL.blend        $= GL.Disabled
                GL.preservingMatrix $ GLUT.renderString GLUT.Roman str
                GL.blend        $= GL.Enabled

        -- colors with float components.
        -- Patched for Borrowed Light: the colour is passed down rather than made
        -- current here; see 'drawPicture'.
        Color col p
         |  stateColor state
         ->  do let RGBA r g b a  = col
                drawPicture ctx circScale (GL.Color4 (gf r) (gf g) (gf b) (gf a)) p

         |  otherwise
         ->     drawPicture ctx circScale colour p


        -- Translation --------------------------
        -- Easy translations are done directly to avoid calling GL.perserveMatrix.
        Translate posX posY (Circle radius)
         -> direct ctx colour >> renderCircle posX posY circScale radius 0

        Translate posX posY (ThickCircle radius thickness)
         -> direct ctx colour >> renderCircle posX posY circScale radius thickness

        Translate posX posY (Arc a1 a2 radius)
         -> direct ctx colour >> renderArc posX posY circScale radius a1 a2 0

        Translate posX posY (ThickArc a1 a2 radius thickness)
         -> direct ctx colour >> renderArc posX posY circScale radius a1 a2 thickness

        -- Patched for Borrowed Light: the transforms push and pop the matrix
        -- with 'GL.unsafePreservingMatrix', which skips the query of the matrix
        -- mode that 'GL.preservingMatrix' makes every time. That is safe here:
        -- drawing a picture never changes the matrix mode, and an exception while
        -- drawing ends the program anyway. 'transformed' also draws the batch
        -- before each push and each pop; see [Note: Batching].
        Translate tx ty (Rotate deg p)
         -> transformed ctx
          $ do  GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
                GL.rotate    (gf deg) (GL.Vector3 0 0 (-1))
                drawPicture ctx circScale colour p

        Translate tx ty p
         -> transformed ctx
          $ do  GL.translate (GL.Vector3 (gf tx) (gf ty) 0)
                drawPicture ctx circScale colour p

        -- Rotation -----------------------------
        -- Easy rotations are done directly to avoid calling GL.perserveMatrix.
        Rotate _   (Circle radius)
         -> direct ctx colour >> renderCircle   0 0 circScale radius 0

        Rotate _   (ThickCircle radius thickness)
         -> direct ctx colour >> renderCircle   0 0 circScale radius thickness

        Rotate deg (Arc a1 a2 radius)
         -> direct ctx colour >> renderArc      0 0 circScale radius (a1-deg) (a2-deg) 0

        Rotate deg (ThickArc a1 a2 radius thickness)
         -> direct ctx colour >> renderArc      0 0 circScale radius (a1-deg) (a2-deg) thickness


        Rotate deg p
         -> transformed ctx
          $ do  GL.rotate (gf deg) (GL.Vector3 0 0 (-1))
                drawPicture ctx circScale colour p

        -- Scale --------------------------------
        Scale sx sy p
         -> transformed ctx
          $ do  GL.scale (gf sx) (gf sy) 1
                let mscale      = max sx sy
                drawPicture ctx (circScale * mscale) colour p

        Bitmap imgData ->
          let (width, height) = bitmapSize imgData
          in
            drawPicture ctx circScale colour $
              BitmapSection (rectAtOrigin width height) imgData

        BitmapSection
          Rectangle
            { rectPos = imgSectionPos
            , rectSize = imgSectionSize }
          imgData@BitmapData
          { bitmapSize = (width, height)
          , bitmapCacheMe = cacheMe }
          ->
           do
            let rowInfo =
                  -- calculate texture coordinates
                  -- remark:
                  --   On some hardware, using exact "integer" coordinates causes texture coords
                  --   with a component == 0  flip to -1. This appears as the texture flickering
                  --   on the left and sometimes show one additional row of pixels outside the
                  --   given rectangle
                  --   To prevent this we add an "epsilon-border".
                  --   This has been testet to fix the problem.
                  let defTexCoords =
                        map (\(x,y) -> (x / fromIntegral width, y / fromIntegral height)) $
                        [ vecMap (+eps) (+eps) $ toFloatVec imgSectionPos
                        , vecMap (subtract eps) (+eps) $ toFloatVec $
                            ( fst imgSectionPos + fst imgSectionSize
                            , snd imgSectionPos )
                        , vecMap (subtract eps) (subtract eps) $ toFloatVec $
                            ( fst imgSectionPos + fst imgSectionSize
                            , snd imgSectionPos + snd imgSectionSize )
                        , vecMap (+eps) (subtract eps) $ toFloatVec $
                            ( fst imgSectionPos
                            , snd imgSectionPos + snd imgSectionSize )
                        ]
                        :: [(Float,Float)]
                      toFloatVec = vecMap fromIntegral fromIntegral
                      vecMap :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
                      vecMap f g (x,y) = (f x, g y)
                      eps = 0.001 :: Float
                  in
                    case rowOrder (bitmapFormat imgData) of
                      BottomToTop -> defTexCoords
                      TopToBottom -> reverse defTexCoords

            -- Patched for Borrowed Light: the batch is drawn first, so the bitmap
            -- lands on top of what was drawn before it.
            flushBatch ctx

            -- Load the image data into a texture,
            -- or grab it from the cache if we've already done that before.
            tex     <- loadTexture (stateTextures state) imgData cacheMe

            -- Set up wrap and filtering mode
            GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
            GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
            GL.textureFilter   GL.Texture2D      $= ((GL.Nearest, Nothing), GL.Nearest)

            -- Enable texturing
            GL.texture GL.Texture2D $= GL.Enabled
            GL.textureFunction      $= GL.Combine

            -- Set current texture
            GL.textureBinding GL.Texture2D $= Just (texObject tex)

            -- Set to opaque. (Patched for Borrowed Light: through 'useColour', so
            -- that what follows knows OpenGL's colour is now white. Gloss used to
            -- restore the previous colour afterwards; now whatever is drawn next
            -- sets the colour it needs.)
            useColour ctx (GL.Color4 1.0 1.0 1.0 1.0)

            -- Draw textured polygon
            GL.renderPrimitive GL.Polygon $
              forM_ (bitmapPath (fromIntegral $ fst imgSectionSize)
                                (fromIntegral $ snd imgSectionSize) `zip` rowInfo) $
              \((polygonCoordX, polygonCoordY), (textureCoordX,textureCoordY)) ->
              do
                GL.texCoord $ GL.TexCoord2 (gf textureCoordX) (gf textureCoordY)
                GL.vertex   $ GL.Vertex2   (gf polygonCoordX) (gf polygonCoordY)

            -- Disable texturing
            GL.texture GL.Texture2D $= GL.Disabled

            -- Free uncachable texture objects.
            freeTexture tex

        Pictures ps
         -> mapM_ (drawPicture ctx circScale colour) ps


-- Batching (patched for Borrowed Light) --------------------------------------
-- | Get OpenGL ready to draw something straight away, not through the batch:
--   draw the batch first, so that what comes next lands on top of it, then make
--   the colour current.
direct :: Ctx -> GL.Color4 GL.GLfloat -> IO ()
direct ctx colour
 = do   flushBatch ctx
        useColour ctx colour
{-# INLINE direct #-}


-- | Make this the colour OpenGL draws with, unless it already is.
useColour :: Ctx -> GL.Color4 GL.GLfloat -> IO ()
useColour ctx colour
 = do   known   <- readIORef (ctxGLColour ctx)
        when (known /= Just colour)
         $ do   GL.currentColor $= colour
                writeIORef (ctxGLColour ctx) (Just colour)


-- | Draw a picture under a pushed matrix: the batch is drawn before the push,
--   with the matrix its triangles were written under, and again before the pop,
--   with the matrix of the picture inside.
transformed :: Ctx -> IO () -> IO ()
transformed ctx action
 = do   flushBatch ctx
        GL.unsafePreservingMatrix
         $ do   action
                flushBatch ctx


-- | Add a convex polygon to the batch as the fan of triangles from its first
--   corner, every vertex in this colour. A polygon with fewer than three corners
--   adds nothing, as GL_POLYGON draws nothing for it.
addFan :: Ctx -> GL.Color4 GL.GLfloat -> [(Float, Float)] -> IO ()
addFan ctx (GL.Color4 r g b a) path
 = case path of
    p0 : p1 : rest@(_ : _)
     -> do  -- n corners make n - 2 triangles, three vertices each.
            let needed  = 3 * length rest
                batch   = stateBatch (ctxState ctx)
            reserve ctx needed
            used    <- readIORef (batchUsed batch)
            ptr     <- readIORef (batchPtr batch)

            let -- Write one vertex, position then colour, into slot i.
                vertex :: Int -> (Float, Float) -> IO ()
                vertex i (x, y)
                 = do   let o = 6 * i
                        pokeElemOff ptr o       (gf x)
                        pokeElemOff ptr (o + 1) (gf y)
                        pokeElemOff ptr (o + 2) r
                        pokeElemOff ptr (o + 3) g
                        pokeElemOff ptr (o + 4) b
                        pokeElemOff ptr (o + 5) a

                -- The triangles (p0, previous, next) along the rest of the path.
                fan :: Int -> (Float, Float) -> [(Float, Float)] -> IO Int
                fan !i previous (next : more)
                 = do   vertex i       p0
                        vertex (i + 1) previous
                        vertex (i + 2) next
                        fan (i + 3) next more
                fan !i _ []
                 =      return i

            end     <- fan used p1 rest
            writeIORef (batchUsed batch) end

    _ -> return ()


-- | Make room in the batch for this many more vertices: draw what is waiting
--   if it would not fit, and grow the buffer if even an empty one is too small.
reserve :: Ctx -> Int -> IO ()
reserve ctx needed
 = do   let batch       = stateBatch (ctxState ctx)
        used    <- readIORef (batchUsed batch)
        cap     <- readIORef (batchCap batch)
        when (used + needed > cap)
         $ do   flushBatch ctx
                when (needed > cap)
                 $ do   let cap'        = max needed (2 * cap)
                        old     <- readIORef (batchPtr batch)
                        free old
                        new     <- mallocArray (6 * cap')
                        writeIORef (batchPtr batch) new
                        writeIORef (batchCap batch) cap'


-- | Draw the triangles waiting in the batch, if any, with one glDrawArrays.
flushBatch :: Ctx -> IO ()
flushBatch ctx
 = do   let batch       = stateBatch (ctxState ctx)
        used    <- readIORef (batchUsed batch)
        when (used > 0)
         $ do   ptr     <- readIORef (batchPtr batch)
                -- Six floats (24 bytes) a vertex: x y at the start, r g b a after.
                GL.arrayPointer GL.VertexArray
                        $= GL.VertexArrayDescriptor 2 GL.Float 24 ptr
                GL.arrayPointer GL.ColorArray
                        $= GL.VertexArrayDescriptor 4 GL.Float 24 (ptr `plusPtr` 8)
                GL.drawArrays GL.Triangles 0 (fromIntegral used)

                -- OpenGL has read the arrays by the time glDrawArrays returns, so
                -- the buffer can be written again at once.
                writeIORef (batchUsed batch) 0

                -- After drawing from a colour array the current colour is undefined.
                writeIORef (ctxGLColour ctx) Nothing

-- Errors ---------------------------------------------------------------------
checkErrors :: String -> IO ()
checkErrors place
 = do   errors          <- get $ GLU.errors
        when (not $ null errors)
         $ mapM_ (handleError place) errors

handleError :: String -> GLU.Error -> IO ()
handleError place err
 = case err of
    GLU.Error GLU.StackOverflow _
     -> error $ unlines
      [ "Gloss / OpenGL Stack Overflow " ++ show place
      , "  This program uses the Gloss vector graphics library, which tried to"
      , "  draw a picture using more nested transforms (Translate/Rotate/Scale)"
      , "  than your OpenGL implementation supports. The OpenGL spec requires"
      , "  all implementations to have a transform stack depth of at least 32,"
      , "  and Gloss tries not to push the stack when it doesn't have to, but"
      , "  that still wasn't enough."
      , ""
      , "  You should complain to your harware vendor that they don't provide"
      , "  a better way to handle this situation at the OpenGL API level."
      , ""
      , "  To make this program work you'll need to reduce the number of nested"
      , "  transforms used when defining the Picture given to Gloss. Sorry." ]

    -- Issue #32: Spurious "Invalid Operation" errors under Windows 7 64-bit.
    --   When using GLUT under Windows 7 it complains about InvalidOperation,
    --   but doesn't provide any other details. All the examples look ok, so
    --   we're just ignoring the error for now.
    GLU.Error GLU.InvalidOperation _
     -> return ()
    _
     -> error $ unlines
     [  "Gloss / OpenGL Internal Error " ++ show place
     ,  "  Please report this on haskell-gloss@googlegroups.com."
     ,  show err ]


-- Textures -------------------------------------------------------------------
-- | Load a texture into the OpenGL context, or retrieve the existing handle
--   from our own cache.
loadTexture
        :: IORef [Texture] -- ^ Existing texture cache.
        -> BitmapData      -- ^ Texture data.
        -> Bool            -- ^ Force cache for newly loaded textures.
        -> IO Texture

loadTexture refTextures imgData@BitmapData{ bitmapSize=(width,height) } cacheMe
 = do   textures        <- readIORef refTextures

        -- Try and find this same texture in the cache.
        name            <- makeStableName imgData
        let mTexCached
                = find (\tex -> texName   tex == name
                             && texWidth  tex == width
                             && texHeight tex == height)
                textures

        case mTexCached of
         Just tex
          ->    return tex

         Nothing
          -> do tex <- installTexture imgData
                when cacheMe
                 $ writeIORef refTextures (tex : textures)
                return tex


-- | Install a texture into the OpenGL context,
--   returning the new texture handle.
installTexture :: BitmapData -> IO Texture
installTexture bitmapData@(BitmapData _ fmt (width,height) cacheMe fptr)
 = do
        let glFormat
                = case pixelFormat fmt of
                        PxABGR -> GL.ABGR
                        PxRGBA -> GL.RGBA

        -- Allocate texture handle for texture
        [tex] <- GL.genObjectNames 1
        GL.textureBinding GL.Texture2D $= Just tex

        -- Sets the texture in imgData as the current texture
        -- This copies the data from the pointer into OpenGL texture memory,
        -- so it's ok if the foreignptr gets garbage collected after this.
        withForeignPtr fptr
         $ \ptr ->
           GL.texImage2D
                GL.Texture2D
                GL.NoProxy
                0
                GL.RGBA8
                (GL.TextureSize2D
                        (gsizei width)
                        (gsizei height))
                0
                (GL.PixelData glFormat GL.UnsignedByte ptr)

        -- Make a stable name that we can use to identify this data again.
        -- If the user gives us the same texture data at the same size then we
        -- can avoid loading it into texture memory again.
        name    <- makeStableName bitmapData

        return  Texture
                { texName       = name
                , texWidth      = width
                , texHeight     = height
                , texData       = fptr
                , texObject     = tex
                , texCacheMe    = cacheMe }


-- | If this texture does not have its `cacheMe` flag set then delete it from
--   OpenGL and free the GPU memory.
freeTexture :: Texture -> IO ()
freeTexture tex
 | texCacheMe tex       = return ()
 | otherwise            = GL.deleteObjectNames [texObject tex]


-- Utils ----------------------------------------------------------------------
-- | Turn alpha blending on or off
setBlendAlpha :: Bool -> IO ()
setBlendAlpha state
        | state
        = do    GL.blend        $= GL.Enabled
                GL.blendFunc    $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

        | otherwise
        = do    GL.blend        $= GL.Disabled
                GL.blendFunc    $= (GL.One, GL.Zero)


-- | Turn line smoothing on or off
setLineSmooth :: Bool -> IO ()
setLineSmooth state
        | state         = GL.lineSmooth $= GL.Enabled
        | otherwise     = GL.lineSmooth $= GL.Disabled


vertexPFs ::    [(Float, Float)] -> IO ()
vertexPFs []    = return ()
vertexPFs ((x, y) : rest)
 = do   GL.vertex $ GL.Vertex2 (gf x) (gf y)
        vertexPFs rest
{-# INLINE vertexPFs #-}

