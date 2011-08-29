module GPipeFPS where

import Graphics.GPipe
import Data.List
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Vec.LinAlg.Transform3D
import qualified Data.Vector as V
import BSPLoader
import qualified Data.Vect as Vect

simple cWorldProjection fb obj = paintColorDepth Less True NoBlending (RGB $ Vec.vec True) (rast obj) fb
  where
--    vert :: (Vec3 (Float),Vec2 (Float),Vec2 (Float),Vec3 (Float),Vec4 (Float)) -> (Vec4 (Vertex Float),Vec4 (Vertex Float))
    vert (v{-,_,_,_,_-}) = (cWorldProjection `multmv` v4,cWorldProjection `multmv` v4)
      where
        v4 = Vec.snoc v 1

    --rast :: PrimitiveStream Triangle (Vec3 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
    rast obj = fmap frag $ rasterizeFront $ fmap vert obj

    --frag :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
    frag (x:.y:.z:.w:.()) = (RGB ((fract' x):.(fract' y):.(1 + fract' z):.()),z / w)

{-
TODO:
    create gpipe mesh from every surfaces

    = DrawVertex
    { dvPosition    :: Vec3
    , dvDiffuseUV   :: Vec2
    , dvLightmaptUV :: Vec2
    , dvNormal      :: Vec3
    , dvColor       :: Vec4
    }

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

data SurfaceType
    = Planar
    | Patch
    | TriangleSoup
    | Flare
-}

-- toIndexedGPUStream
--geometry :: BSPLevel -> V.Vector (PrimitiveStream Triangle (Vec3 (Vertex Float),Vec2 (Vertex Float),Vec2 (Vertex Float),Vec3 (Vertex Float),Vec4 (Vertex Float)))
geometry bsp = V.map convertSurface $ blSurfaces bsp    
  where
    convertSurface sf = case srSurfaceType sf of
        Planar       -> toIndexedGPUStream TriangleList v i
        --Patch        -> toGPUStream Point v
        TriangleSoup -> toIndexedGPUStream TriangleList v i
        --Flare        -> toGPUStream Point v
        _              -> toGPUStream TriangleList []
      where
        v = V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) vertices
        i = V.toList $ V.take (srNumIndices sf) $ V.drop (srFirstIndex sf) indices
    vertices = V.map convertVertex $ blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p{-, v2 dt, v2 lt, v3 n, v4 c-})
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = i:.j:.k:.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()

renderBSP worldProjection bsp = V.foldl' (simple worldProjection) clear $ geometry bsp
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