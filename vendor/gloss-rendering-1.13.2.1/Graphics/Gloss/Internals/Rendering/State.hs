{-# OPTIONS_HADDOCK hide #-}

-- | Rendering options
module Graphics.Gloss.Internals.Rendering.State
        ( State (..)
        , initState
        , Texture (..)
        , Batch (..)
        , TessCache (..))
where
import Graphics.Gloss.Internals.Data.Picture
import Foreign.ForeignPtr
import Foreign.Marshal.Array    (mallocArray)
import Foreign.Ptr              (Ptr)
import System.Mem.StableName
import Data.Word
import Data.IORef
import qualified Data.IntMap.Strict             as IntMap
import qualified Graphics.Rendering.OpenGL.GL   as GL


-- | Abstract Gloss render state which holds references to textures
--   loaded into the GPU context.
data State
        = State
        { -- | Whether to use color
          stateColor            :: !Bool

        -- | Whether to force wireframe mode only
        , stateWireframe        :: !Bool

        -- | Whether to use alpha blending
        , stateBlendAlpha       :: !Bool

        -- | Whether to use line smoothing
        , stateLineSmooth       :: !Bool
        
        -- | Cache of Textures that we've sent to OpenGL.
        , stateTextures         :: !(IORef [Texture])

        -- | Patched for Borrowed Light: convex polygons waiting to be drawn
        --   together (see 'Batch'). Kept here so the buffer is allocated once,
        --   not once a frame.
        , stateBatch            :: !Batch

        -- | Patched for Borrowed Light: the triangles GLU made of recently drawn
        --   polygons that are not convex (see 'TessCache').
        , stateTessCache        :: !(IORef TessCache)
        }
        

-- | A texture that we've sent to OpenGL.
data Texture
        = Texture
        { -- | Stable name derived from the `BitmapData` that the user gives us.
          texName       :: StableName BitmapData

        -- | Width of the image, in pixels.
        , texWidth      :: Int

        -- | Height of the image, in pixels.
        , texHeight     :: Int

        -- | Pointer to the Raw texture data.
        , texData       :: ForeignPtr Word8
        
        -- | The OpenGL texture object.
        , texObject     :: GL.TextureObject

        -- | Whether we want to leave this in OpenGL texture memory between frames.
        , texCacheMe    :: Bool }


-- | Patched for Borrowed Light: triangles collected from convex polygons, to be
--   drawn with one @glDrawArrays@ call instead of one @glBegin@/@glEnd@ pair
--   (and one call per corner) for every polygon. See [Note: Batching] in
--   "Graphics.Gloss.Internals.Rendering.Picture".
--
--   The buffer is plain C memory holding six 'GL.GLfloat's per vertex: the
--   position @x y@, then the colour @r g b a@. Each vertex carries its own colour,
--   so polygons of different colours can share a batch. The colour written is
--   exactly the one Gloss would have made current with @glColor4f@ before drawing
--   that polygon, so OpenGL sees the same values per vertex either way.
data Batch
        = Batch
        { -- | Start of the buffer.
          batchPtr      :: !(IORef (Ptr GL.GLfloat))

          -- | How many vertices the buffer has room for.
        , batchCap      :: !(IORef Int)

          -- | How many vertices are written and waiting to be drawn.
        , batchUsed     :: !(IORef Int) }


-- | Patched for Borrowed Light: GLU's triangles for polygons that are not
--   convex, so a polygon drawn again next frame is not tessellated again.
--
--   The game draws the same prebuilt (and so the very same heap object) polygon
--   every frame, and GLU spent about a millisecond a frame re-tessellating the
--   same hundred shapes. GLU is deterministic, so drawing its stored output
--   issues exactly the calls that tessellating again would.
--
--   A polygon is recognised by the 'StableName' of its list of points, which
--   identifies that one heap object (never merely an equal list). The key of
--   each map is 'hashStableName'; names that share a hash share a bucket.
--
--   Entries live for two frames: 'tessFresh' holds what was drawn this frame,
--   'tessStale' what was drawn the frame before. At the end of a frame the fresh
--   map becomes the stale one, so the cache never holds more than two frames'
--   polygons, and shapes rebuilt every frame are dropped after one frame.
data TessCache
        = TessCache
        { tessFresh     :: !(IntMap.IntMap [(StableName [(Float, Float)], [GL.Vertex3 GL.GLdouble])])
        , tessStale     :: !(IntMap.IntMap [(StableName [(Float, Float)], [GL.Vertex3 GL.GLdouble])]) }


-- | A mutable render state holds references to the textures currently loaded
--   into the OpenGL context. To ensure that textures are cached in GPU memory,
--   pass the same `State` each time you call `displayPicture` or `renderPicture`.
initState :: IO State
initState
 = do   textures        <- newIORef []

        -- Patched for Borrowed Light: room for 16384 vertices to start with
        -- (384 KiB). A batch that needs more is drawn early, and a single polygon
        -- that needs more makes the buffer grow; see 'reserve' in
        -- "Graphics.Gloss.Internals.Rendering.Picture".
        let capacity    = 16384
        buffer          <- mallocArray (capacity * 6)
        batch           <- Batch <$> newIORef buffer <*> newIORef capacity <*> newIORef 0
        tess            <- newIORef (TessCache IntMap.empty IntMap.empty)

        return  State
                { stateColor            = True
                , stateWireframe        = False
                , stateBlendAlpha       = True
                , stateLineSmooth       = False 
                , stateTextures         = textures
                , stateBatch            = batch
                , stateTessCache        = tess }
        
