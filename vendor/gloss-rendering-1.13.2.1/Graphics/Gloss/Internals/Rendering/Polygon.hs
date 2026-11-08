{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_HADDOCK hide #-}

-- Patched for Borrowed Light: this module used to draw a polygon itself
-- ('renderComplexPolygon'): GL_POLYGON when 'isConvex' said so, GLU's
-- triangles otherwise. Drawing now happens in
-- "Graphics.Gloss.Internals.Rendering.Picture", which batches the convex ones;
-- this module keeps the test that decides, made cheaper without changing a single
-- answer, and keeps GLU's triangles from one frame to the next.
module Graphics.Gloss.Internals.Rendering.Polygon
        ( isConvex
        , isConvexOriginal
        , tessellation
        , ageTessCache )
where
import Graphics.Gloss.Internals.Rendering.State
import Graphics.Rendering.OpenGL.GLU.Tessellation
import Data.IORef
import System.Mem.StableName
import qualified Data.IntMap.Strict                     as IntMap
import qualified Graphics.Rendering.OpenGL.GL           as GL

combiner :: a -> b -> ()
combiner _ _ = ()


-- written this way to measurably improve performance
zipLoop :: [a] -> [(a,a)]
zipLoop [] = []
zipLoop (x:xs) = go x xs where
  go y [] = [(y,x)]
  go y (z:rs) = (y,z) : go z rs

-- signed angle between 2 vectors
-- https://stackoverflow.com/a/16544330/1779797
-- Note: `isConvex` would remain correct if this returned a value between theta*7/2pi and theta*7/4pi which probably provides an opportunity for optimisation
angle :: (Float,Float) -> (Float,Float) -> Float
angle (x1,y1) (x2,y2) = let dot = x1*x2 + y1*y2 -- cos theta * |v1||v2|
                            det = y2*x1 - x2*y1 -- sin theta * |v1||v2|
                        in atan2 det dot


-- | Whether the polygon is convex, so that it can be drawn as it stands rather
--   than cut into triangles by GLU.
--
--   Patched for Borrowed Light: the answer is exactly the one Gloss's original
--   test gives (kept below as 'isConvexOriginal', the specification), for every
--   list of points; only the time it takes changes. It used to take about a
--   millisecond a frame for the game's four thousand polygons.
--
--   * 'quickConvex' answers True at once, without 'atan2', for the commonest
--     shapes: triangles and quadrilaterals that turn clearly the same way at
--     every corner. It only ever says True where the original says True too.
--
--   * Everything else gets 'exactConvex': the original computation, the same
--     floating-point operations in the same order, written as one strict loop
--     instead of a chain of lazy lists.
isConvex :: [(Float,Float)] -> Bool
isConvex ps = quickConvex ps || exactConvex ps
{-# INLINE isConvex #-}


-- | Gloss's original test, unchanged: the specification that 'isConvex' meets.
--   Not used when drawing; exported so it stays compiled next to the version
--   that is.
--
--   Approximating 2pi by 7 is reasonable here.
--   The total rotation theoretically must be a multiple of 2pi,
--   so a little generosity doesn't break anything, but might save some cases that would fail due to floating point errors
isConvexOriginal :: [(Float,Float)] -> Bool
isConvexOriginal ps =
    -- check that it doesn't turn more than one full circle in total
    all (\theta -> (theta <= 7) && (theta > -7)) $
    scanl angleAdd 0 $ -- check that the path's direction is consistent
    zipWithLoop angle $ -- compute angles of turns at each vertex
    keptEdges ps
  where
    zipWithLoop :: (a->a->b) -> [a] -> [b]
    zipWithLoop f = map (uncurry f) . zipLoop


-- | The polygon's edges as vectors, from each point to the next and from the
--   last back to the first, leaving out edges of zero length (which repeated
--   points produce). Exactly the edges the original test turns at.
keptEdges :: [(Float,Float)] -> [(Float,Float)]
keptEdges ps =
    filter (/= (0,0)) $ -- discard edges arising from duplicated vertices
    map (\((x1,y1),(x2,y2)) -> (x2-x1,y2-y1)) $ -- produce vectors for each edge
    zipLoop ps


-- Combine angles, but return a value greater than 7 if they have opposite signs
angleAdd :: Float -> Float -> Float
angleAdd a b = if signum a*signum b < -0.5 then 10 else a + b


-- | Patched for Borrowed Light: the original test, as one strict loop.
--
--   The original builds the list of turns @[angle k0 k1, angle k1 k2, ...,
--   angle kLast k0]@ between consecutive kept edges, runs 'angleAdd' along it
--   from 0 with 'scanl', and asks that every running total (the leading 0
--   included, which always passes) lie in (-7, 7]. This loop computes the same
--   turns with the same 'angle', adds them with the same 'angleAdd' in the same
--   order, and checks each running total as it goes, so it returns the same Bool;
--   it just stops at the first total out of range and allocates no lists of
--   turns or totals.
exactConvex :: [(Float,Float)] -> Bool
exactConvex ps = case keptEdges ps of
    []          -> True                 -- no turns: only the leading 0
    (k0 : ks)   -> go k0 0 k0 ks
  where
    -- @go first total previous rest@: @total@ is the running total so far, and
    -- @previous@ the edge whose turn into the next one comes next.
    go :: (Float,Float) -> Float -> (Float,Float) -> [(Float,Float)] -> Bool
    go !k0 !total prev (e : es)
     = let total' = angleAdd total (angle prev e)
       in  inRange total' && go k0 total' e es
    -- Last of all, the turn from the last edge back into the first.
    go !k0 !total prev []
     = inRange (angleAdd total (angle prev k0))

    inRange :: Float -> Bool
    inRange theta = (theta <= 7) && (theta > -7)


-- | Patched for Borrowed Light: True only for a triangle or quadrilateral that
--   the original test certainly calls convex, decided without 'atan2'. False
--   means "not decided here", not "not convex". It settles about 94% of the
--   polygons in the game's frames.
--
--   Every corner (the turn from one edge into the next, the last edge into the
--   first included) is judged by the same @det@ and @dot@ that 'angle' computes,
--   and must
--
--   * have both of them finite and below 1e30 in size;
--   * turn by a clear amount: @det > 1e-30@ (or @det < -1e-30@ for the other
--     way round), all corners turning the same way;
--   * turn by less than a right angle (@dot >= 0@), or else stay clear of a
--     full reversal: @|det| > 0.001 * |dot|@, a turn at least 0.001 radians
--     short of pi.
--
--   Why the original then says True as well:
--
--   1. No edge has zero length: a zero edge makes @det@ 0 at both its corners,
--      which fails the second rule. So the original keeps every edge, and its
--      turns are exactly the corners judged here.
--
--   2. Each turn it computes, @atan2 det dot@, has the sign of @det@ (or is 0):
--      all turns have the same sign, so 'angleAdd' never produces its "opposite
--      signs" value 10, and the running totals only grow (or only shrink) to the
--      final sum. Only that sum needs bounding.
--
--   3. Let phi be the exact turns between the same float edge vectors, computed
--      without rounding. Rounding makes @(det, dot)@ off by at most 2^-22.5 of
--      its length (|x1*y2| + |y1*x2| and |x1*x2| + |y1*y2| are both at most
--      |e1|*|e2|, which is that length; the 1e-30 floor keeps the absolute error
--      of tiny products negligible, and the 1e30 ceiling rules out overflow), and
--      'atan2' adds a few units in the last place, so each computed turn is
--      within 1e-6 of its phi. The third rule keeps every computed turn at least
--      0.001 short of pi in size, so every phi is short of pi too (and none is
--      past zero by more than 1e-6). The phis of a closed polygon add up to a
--      whole number of turns, 2*pi*k; with three or four of them, each between
--      -1e-6 and pi, that is 0 or 2*pi.
--
--   4. So the computed sum is within 1e-5 of 0 or of 2*pi = 6.2832: inside
--      (-7, 7], and the original says True.
--
--   With five or more corners the turns could add up to 4*pi (a star), so those
--   always go to 'exactConvex'. A NaN anywhere fails every comparison and goes
--   there too.
quickConvex :: [(Float,Float)] -> Bool
quickConvex ps = case ps of
    [(x0,y0), (x1,y1), (x2,y2)]
     -> let -- the edges, as 'keptEdges' computes them
            ax = x1-x0; ay = y1-y0
            bx = x2-x1; by = y2-y1
            cx = x0-x2; cy = y0-y2
        in  sameTurns3 (corner ax ay bx by) (corner bx by cx cy) (corner cx cy ax ay)

    [(x0,y0), (x1,y1), (x2,y2), (x3,y3)]
     -> let ax = x1-x0; ay = y1-y0
            bx = x2-x1; by = y2-y1
            cx = x3-x2; cy = y3-y2
            dx = x0-x3; dy = y0-y3
        in  sameTurns4 (corner ax ay bx by) (corner bx by cx cy)
                       (corner cx cy dx dy) (corner dx dy ax ay)

    _ -> False
  where
    -- +1 for a clear turn one way that is clear of a reversal, -1 the same the
    -- other way, 0 for anything else.
    corner :: Float -> Float -> Float -> Float -> Int
    corner x1 y1 x2 y2
        | not (abs det < 1e30 && abs dot < 1e30)               = 0
        | det >  1e-30, dot >= 0 || det > 0.001 * negate dot    = 1
        | det < -1e-30, dot >= 0 || negate det > 0.001 * negate dot = -1
        | otherwise                                             = 0
        where dot = x1*x2 + y1*y2   -- as in 'angle'
              det = y2*x1 - x2*y1

    sameTurns3 :: Int -> Int -> Int -> Bool
    sameTurns3 a b c = a /= 0 && a == b && a == c

    sameTurns4 :: Int -> Int -> Int -> Int -> Bool
    sameTurns4 a b c d = a /= 0 && a == b && a == c && a == d


-- | GLU's triangles for a polygon that is not convex, as the vertices to give
--   @glVertex3d@ in order, three to a triangle.
--
--   Patched for Borrowed Light: looked up in the 'TessCache' first. The list is
--   the one Gloss used to hand to OpenGL straight after tessellating: the same
--   'triangulate' call with the same arguments, and its triangles' corners in
--   the same order.
tessellation :: IORef TessCache -> [(Float,Float)] -> IO [GL.Vertex3 GL.GLdouble]
tessellation cacheRef path
 = do   -- By now 'isConvex' has evaluated the list, so this names the evaluated
        -- list itself (the runtime looks through the indirection a thunk leaves).
        name    <- makeStableName path
        cache   <- readIORef cacheRef
        let key = hashStableName name
            find m = IntMap.lookup key m >>= lookup name

        case find (tessFresh cache) of
         Just vs -> return vs
         Nothing
          -> do vs <- case find (tessStale cache) of
                        Just vs -> return vs
                        Nothing -> triangulateGLU path
                writeIORef cacheRef cache
                        { tessFresh = IntMap.insertWith (++) key [(name, vs)] (tessFresh cache) }
                return vs


-- | Patched for Borrowed Light: call once at the end of every frame. What was
--   drawn this frame is kept for the next; what was not drawn this frame is
--   forgotten.
ageTessCache :: IORef TessCache -> IO ()
ageTessCache cacheRef
 = modifyIORef' cacheRef (\cache -> TessCache IntMap.empty (tessFresh cache))


-- | Gloss's tessellation, as it was in 'renderComplexPolygon': odd winding, no
--   tolerance, the normal along +z. The result is fully evaluated so that the
--   cache keeps only the vertices, not GLU's intermediate structures.
triangulateGLU :: [(Float,Float)] -> IO [GL.Vertex3 GL.GLdouble]
triangulateGLU path
 = do   Triangulation ts <- triangulate TessWindingOdd 0 ( GL.Normal3 0 0 1) combiner
          (ComplexPolygon [ComplexContour [AnnotatedVertex (GL.Vertex3 (realToFrac a) (realToFrac b) 0) () | (a,b) <- path]])
        let vs = [ v | Triangle (AnnotatedVertex v1 _) (AnnotatedVertex v2 _) (AnnotatedVertex v3 _) <- ts
                     , v <- [v1, v2, v3] ]
        foldr (\(GL.Vertex3 x y z) rest -> x `seq` y `seq` z `seq` rest) (return vs) vs
