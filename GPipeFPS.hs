module GPipeFPS where

import BSPLoader
import Data.List
import Data.Trie (Trie)
import Data.Vec.LinAlg.Transform3D
import Data.Vec.Nat
import Foreign
import Graphics.GPipe
import qualified Data.ByteString as SB
import qualified Data.Trie as T
import qualified Data.Vec as Vec
import qualified Data.Vect as Vect
import qualified Data.Vector as V

{-
data Surface
    = Surface
    { srShaderNum      :: Int
    , srFogNum         :: Int
    , srSurfaceType    :: SurfaceType
    , srFirstVertex    :: Int
    , srNumVertices    :: Int
    , srFirstIndex     :: Int
    , srNumIndices     :: Int
    , srLightmapNum    :: Int
    , srLightmapPos    :: Vec2
    , srLightmapSize   :: Vec2
    , srLightmapOrigin :: Vec3
    , srLightmapVec1   :: Vec3
    , srLightmapVec2   :: Vec3
    , srLightmapVec3   :: Vec3
    , srPatchSize      :: Vec2
    }
-}

type VertexData = (Vec.Vec3 (Vertex Float),Vec.Vec2 (Vertex Float),Vec.Vec4 (Vertex Float))
type Mesh = PrimitiveStream Triangle VertexData
type FB = FrameBuffer RGBFormat DepthFormat ()

-- time -> worldProjection -> inFrameBuffer -> resultFrameBuffer
type SurfaceRenderer = Vertex Float -> Vec.Mat44 (Vertex Float) -> FB -> FB
type Renderer = Mesh -> SurfaceRenderer

errorRenderer :: Maybe (Texture2D RGBFormat) -> Renderer
errorRenderer t obj time cWorldProjection fb = paintColorDepth Less True NoBlending (RGB $ Vec.vec True) (rast obj) fb
  where
    vert (v,lt,c) = (cWorldProjection `multmv` v4,(cWorldProjection `multmv` v4,lt,c))
      where
        v4 = Vec.snoc v 1

    rast obj = fmap frag $ rasterizeBack $ fmap vert obj

    frag (x:.y:.z:.w:.(),lt,cr:.cg:.cb:.ca:.()) = (RGB ((cr * r * fract' x):.(cg * g * fract' y):.(cb * b * (1 + fract' z)):.()),z / w)
      where
        RGB (r:.g:.b:.()) = case t of
            Just tx -> RGB (1:.1:.1:.())--sample (Sampler Linear Clamp) tx lt
            Nothing -> RGB (1:.1:.1:.())

compileBSP :: Trie Renderer -> BSPLevel -> V.Vector SurfaceRenderer
compileBSP shaderMap bsp = V.map convertSurface $ blSurfaces bsp
  where
    shaders = V.map (\k -> T.lookup (shName k) shaderMap) $ blShaders bsp
    lightmaps = V.map tx $ blLightmaps bsp

    tx :: Lightmap -> Texture2D RGBFormat
    tx lm = unsafePerformIO $ SB.useAsCString (lmMap lm) $ \p -> newTexture (PerComp3 UnsignedByteFormat) RGB8 (128:.128:.()) [castPtr p]

    convertSurface sf = renderer $ case srSurfaceType sf of
        Planar       -> toIndexedGPUStream TriangleList v i
        TriangleSoup -> toIndexedGPUStream TriangleList v i
        --Patch        -> toGPUStream Point v
        --Flare        -> toGPUStream Point v
        _              -> toGPUStream TriangleList []
      where
        v = V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) vertices
        i = V.toList $ V.take (srNumIndices sf) $ V.drop (srFirstIndex sf) indices
        renderer = case shaders V.! srShaderNum sf of
            Just r  -> r
            Nothing -> errorRenderer lm
        lm = if 0 <= lmidx && lmidx < V.length lightmaps then Just $ lightmaps V.! lmidx else Nothing
        lmidx = srLightmapNum sf

    vertices = V.map convertVertex $ blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p,v2 lt,v4 c)-- , v2 dt, v2 lt, v3 n, v4 c)
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = (s i):.(s j):.(s k):.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()
    s a = 0.01 * a

{-
geometry :: BSPLevel -> V.Vector (PrimitiveStream Triangle (Vec3 (Vertex Float)))
geometry bsp = V.fromList [toGPUStream TriangleList $ V.toList $ V.map convertVertex $ V.concatMap convertSurface $ blSurfaces bsp]
  where
    convertSurface sf = case srSurfaceType sf of
        Planar       -> V.backpermute v i
        TriangleSoup -> V.backpermute v i
        _            -> V.empty
      where
        v = V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) vertices
        i = V.take (srNumIndices sf) $ V.drop (srFirstIndex sf) indices
    vertices = blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p)--, v2 dt, v2 lt, v3 n, v4 c)
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = (s i):.(s j):.(s k):.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()
    s a = 0.01 * a
-}

renderSurfaces :: Vertex Float -> Vec.Mat44 (Vertex Float) -> V.Vector SurfaceRenderer -> FB
renderSurfaces time worldProjection faces = V.foldl' (\fb fun -> fun time worldProjection fb) clear $ faces
  where
    clear = newFrameBufferColorDepth (RGB (0:.0:.0:.())) 1000

{-
-- first we should render all faces
drawBSPLevel ltl tl bl cp fr = do
    let l   = findLeafIdx bl cp 0
        lf  = (blLeafs bl) V.! l
        clI = lfCluster lf
        lfs = V.filter (\a -> (isClusterVisible bl clI $ lfCluster a) && cutLf a) $ blLeafs bl
        cutLf a = boxInFrustum (lfMaxs a) (lfMins a) fr
        fac = UV.replicate (V.length $ blFaces bl) False
        fac' = V.foldr (visitLeaf bl) fac lfs
        fidx = V.fromList $ UV.toList $ UV.elemIndices True fac'

        faces = case ( l < 0 || l >= V.length (blLeafs bl)) of
            True    -> V.toList $ blFaces bl -- render all faces, because we are out from bsp tree
            False   -> V.toList $ V.map (\i -> (blFaces bl) V.! i) fidx

    forM_ faces $ \f -> when (fcType f == 1 || fcType f == 3 ) $ drawFace ltl tl bl f

visitLeaf bl lf fac = UV.update_ fac idx $ UV.replicate (UV.length idx) True
  where
    lfaces = blSurfaces bl
    ff  = lfFirstLeafSurface lf
    fc  = lfNumLeafSurfaces lf
    idx = UV.take fc $ UV.drop ff lfaces

drawSurface ltl tl bl f = do
    let voffset = srFirstVertex f
        vcount  = srNumVertices f
        foffset = srFirstIndex f
        fcount  = srNumIndices f
        verts   = blDrawVertices bl
        tris    = blDrawIndices bl
-}