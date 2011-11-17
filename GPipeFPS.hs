module GPipeFPS where

import BSPLoader
import Data.List
import Data.Trie (Trie)
import Data.Vec.LinAlg.Transform3D
import Data.Vec.Nat
import Foreign
import Graphics.GPipe
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Vec as Vec
import qualified Data.Vect as Vect
import qualified Data.Vector as V

import GPipeUtils
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

type VertexData = (Vec.Vec3 (Vertex Float),Vec.Vec2 (Vertex Float),Vec.Vec2 (Vertex Float),Vec.Vec4 (Vertex Float))
type Mesh = PrimitiveStream Triangle VertexData
type FB = FrameBuffer RGBAFormat DepthFormat ()

-- time -> worldProjection -> inFrameBuffer -> resultFrameBuffer
type SurfaceRenderer = Vertex Float -> Vec.Mat44 (Vertex Float) -> FB -> FB
type Renderer = Mesh -> SurfaceRenderer

rendererSkeleton :: Mesh -> Vertex Float -> Vec.Mat44 (Vertex Float) -> FB -> FB
rendererSkeleton obj time cWorldProjection fb = undefined

errorRenderer :: Maybe (Texture2D RGBAFormat) -> Maybe (Texture2D RGBAFormat) -> Renderer
errorRenderer tx ltx obj time cWorldProjection fb = paintColorRastDepth Less True NoBlending (RGBA (Vec.vec True) True) (rast obj) fb
  where
    vert (v,dt,lt,c) = (cWorldProjection `multmv` v4,(cWorldProjection `multmv` v4,dt,lt,c))
      where
        v4 = Vec.snoc v 1

    rast obj = fmap frag $ rasterizeBack $ fmap vert obj

    frag (x:.y:.z:.w:.(),dt,lt,cr:.cg:.cb:.ca:.()) = RGBA ((r * fract' x):.(g * fract' y):.(b * (1 + fract' z)):.()) 1
      where
        RGBA (r:.g:.b:.()) a = case tx of
            Just txo -> sample (Sampler Linear Wrap) txo lt
            Nothing -> RGBA (1:.1:.1:.()) 1

type VertexDeformer = Vertex Float -> Vec.Vec3 (Vertex Float) -> Vec.Vec3 (Vertex Float)

data CommonAttrs
    = CommonAttrs
    { caSkyParms        :: () -- TODO
    , caFogParms        :: () -- TODO
    , caPortal          :: Bool
    , caSort            :: Int -- default: 3 or 6 depends on blend function
    , caEntityMergable  :: Bool
    , caFogOnly         :: Bool
    , caCull            :: () -- TODO, default = front
    , caDeformVertexes  :: [VertexDeformer]
    , caNoMipMaps       :: Bool
    , caPolygonOffset   :: Maybe Float
    , caRenderers       :: [Renderer]
    }

defaultCommonAttrs :: CommonAttrs
defaultCommonAttrs = CommonAttrs
    { caSkyParms        = ()
    , caFogParms        = ()
    , caPortal          = False
    , caSort            = 0
    , caEntityMergable  = False
    , caFogOnly         = False
    , caCull            = ()
    , caDeformVertexes  = []
    , caNoMipMaps       = False
    , caPolygonOffset   = Nothing
    , caRenderers       = []
    }

data StageAttrs
    = StageAttrs
    { saTextureSrc  :: Texture2D RGBAFormat
    , saBlend       :: Blending
    , saRGBGen      :: () -- texture, vertex color, constant
    , saAlphaGen    :: () -- texture, vertex color, constant
    , saTCGen       :: () -- diffuse tex, lightmap tex, env mapping
    , saTCMod       :: [()] -- TODO
    , saDepthWrite  :: Bool
    , saDepthFunc   :: ComparisonFunction
    , saAlphaFunc   :: ()
    }

defaultStageAttrs :: StageAttrs
defaultStageAttrs = StageAttrs
    { saTextureSrc  = whiteImage
    , saBlend       = NoBlending
    , saRGBGen      = ()
    , saAlphaGen    = ()
    , saTCGen       = ()
    , saTCMod       = []
    , saDepthWrite  = False
    , saDepthFunc   = Lequal
    , saAlphaFunc   = ()
    }

addRenderer ca sa = ca {caRenderers = r:caRenderers ca}
  where
    r = textureRenderer $ saTextureSrc sa

{-
    structure of a q3 shader
    
    blend, depth func, depth mask : parameter of paintColorRastDepth
    
    vertex transformation
    
    fragment:
        rgb gen
            - texture
            - vertex color (with variations)
                hint: calculation should be implemented in vertex function and in faragment side it should be an identity function
            - constant (with variations)
        alpha gen
            - 
    texture coord generation
        - diffuse texture coords
        - lightmap texture coords
        - enviroment mapped texture coords
    
-}
{-
    factor out:
        paint (blend, depthfunc, deptwrite)
        vertex transformation
        rasterizeBack (cull)
        vert (vertex transformation, we should keep only texcoord gen)

    (hint) for vertex transformation use function like: Vec.Vec3 (Vertex Float) -> Vec.Vec3 (Vertex Float)
    (hint) for texture coord transformations use function like: Vec.Vec2 (Vertex Float) -> Vec.Vec2 (Vertex Float)
    use case:
        - create gpipe texture (according source and mipmapping configuration)
        - create texture selector function: time -> [texture] -> texture
        - create texture renderer:
            texture selector
            textures
            vertex transformator function
            texcoord transformator function
-}
textureRenderer :: Texture2D RGBAFormat -> Renderer
textureRenderer tx obj time cWorldProjection fb = paintColorRastDepth Less True NoBlending (RGBA (Vec.vec True) True) (rast obj) fb
  where
    vert (v,dt,lt,c) = (cWorldProjection `multmv` v4,(cWorldProjection `multmv` v4,dt,lt,c))
      where
        v4 = Vec.snoc v 1

    rast obj = fmap frag $ rasterizeBack $ fmap vert obj
    frag (x:.y:.z:.w:.(),dt,lt,cr:.cg:.cb:.ca:.()) = sample (Sampler Linear Wrap) tx dt

stagesRenderer :: [Renderer] -> Renderer
stagesRenderer stages obj time cWorldProjection fb = foldl' (\f r -> r obj time cWorldProjection f) fb stages

renderSurfaces :: Vertex Float -> Vec.Mat44 (Vertex Float) -> V.Vector SurfaceRenderer -> FB
renderSurfaces time worldProjection faces = V.foldl' (\fb fun -> fun time worldProjection fb) clear $ faces
  where
    clear = newFrameBufferColorDepth (RGBA (0:.0:.0:.()) 1) 1000

compileBSP :: Trie Renderer -> BSPLevel -> V.Vector SurfaceRenderer
compileBSP shaderMap bsp = V.map convertSurface $ blSurfaces bsp
  where
    myLookup k = unsafePerformIO $ do
        case T.lookup (shName k) shaderMap of
            Nothing -> putStrLn ("shader not found: " ++ show (shName k)) >> return Nothing
            Just v -> return $ Just v
    myLookup' k = case T.lookup (shName k) shaderMap of
        Nothing -> Just $ textureRenderer $ loadQ3Texture $ SB.unpack $ shName k
        Just v -> Just v
    shaders = V.map myLookup' $ blShaders bsp
    lightmaps = V.map tx $ blLightmaps bsp

    tx :: Lightmap -> Texture2D RGBAFormat
    tx lm = textureFromByteString 3 128 128 $ lmMap lm

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
            Nothing -> errorRenderer Nothing lm
        lm = if 0 <= lmidx && lmidx < V.length lightmaps then Just $ lightmaps V.! lmidx else Nothing
        lmidx = srLightmapNum sf

    vertices = V.map convertVertex $ blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p,v2 dt,v2 lt,v4 c)-- , v2 dt, v2 lt, v3 n, v4 c)
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = (s i):.(s j):.(s k):.()
    v3' (Vect.Vec3 i j k) = i:.j:.k:.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()
    s a = 0.01 * a

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
