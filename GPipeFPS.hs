module GPipeFPS where

import BSPLoader
import Control.Monad
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
import qualified Data.Vector.Mutable as MV

import GPipeUtils

type VertexData = (Vec.Vec3 (Vertex Float),{-Vec.Vec3 (Vertex Float), -}Vec.Vec2 (Vertex Float),Vec.Vec2 (Vertex Float),Vec.Vec4 (Vertex Float))
type Mesh = PrimitiveStream Triangle VertexData
type FB = FrameBuffer RGBAFormat DepthFormat ()

-- time -> worldProjection -> inFrameBuffer -> resultFrameBuffer
type SurfaceRenderer = Float -> Vertex Float -> Vec.Mat44 (Vertex Float) -> FB -> FB
type Renderer = Texture2D RGBAFormat -> Mesh -> SurfaceRenderer

type RGBFun    = Vertex Float -> VertexData -> Vec.Vec3 (Vertex Float)
type AlphaFun  = Vertex Float -> VertexData -> Vertex Float
type TCFun     = Vertex Float -> VertexData -> Vec.Vec2 (Vertex Float)
type TexFun    = Texture2D RGBAFormat -> Float -> Texture2D RGBAFormat
type SampleFun = Texture2D RGBAFormat -> Vec.Vec2 (Fragment Float) -> Color RGBAFormat (Fragment Float)

type VertexDeformer = Vertex Float -> Vec.Vec3 (Vertex Float) -> Vec.Vec3 (Vertex Float)

identityLight :: Float
identityLight = 1

data Entity
    = Entity
    { eAmbientLight     :: Vec.Vec4 (Vertex Float)
    , eDirectedLight    :: Vec.Vec4 (Vertex Float)
    , eLightDir         :: Vec.Vec3 (Vertex Float)
    , eShaderRGBA       :: Vec.Vec4 (Vertex Float)
    }

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
    , caSort            = 3
    , caEntityMergable  = False
    , caFogOnly         = False
    , caCull            = ()
    , caDeformVertexes  = []
    , caNoMipMaps       = False
    , caPolygonOffset   = Nothing
    , caRenderers       = []
    }

data StageTexture
    = ST_Map        SB.ByteString
    | ST_ClampMap   SB.ByteString
    | ST_AnimMap    Float [SB.ByteString]
    | ST_Lightmap
    | ST_WhiteImage

data StageAttrs
    = StageAttrs
    { saBlend       :: Maybe (BlendingFactor,BlendingFactor)
    , saRGBGen      :: RGBFun
    , saAlphaGen    :: AlphaFun
    , saTCGen       :: VertexData -> Vec.Vec2 (Vertex Float)
    , saTCMod       :: [Vertex Float -> Vec.Vec2 (Vertex Float) -> Vec.Vec2 (Vertex Float)]
    , saTexture     :: StageTexture
    , saDepthWrite  :: Bool
    , saDepthFunc   :: ComparisonFunction
    , saAlphaFunc   :: ()
    }

defaultStageAttrs :: StageAttrs
defaultStageAttrs = StageAttrs
    { saBlend       = Nothing
    , saRGBGen      = \_ _ -> 1:.1:.1:.()
    , saAlphaGen    = \_ _ -> 1
    , saTCGen       = \(_,uv,_,_) -> uv
    , saTCMod       = []
    , saTexture     = ST_WhiteImage
    , saDepthWrite  = False
    , saDepthFunc   = Lequal
    , saAlphaFunc   = ()
    }

addRenderer ca sa = ca {caRenderers = r:caRenderers ca}
  where
    r = stageRenderer (saDepthFunc sa) depthWrite blend vertexFun (saRGBGen sa) (saAlphaGen sa) tcFun texFun sampleFun
    mipmap = not $ caNoMipMaps ca
    vertexFun t v = v
    tcFun t vd = foldl' (\uv f -> f t uv) (saTCGen sa vd) (reverse $ saTCMod sa)
    depthWrite = if NoBlending == blend then True else True --saDepthWrite sa
    blend = case saBlend sa of
        Nothing -> NoBlending
        Just b  -> Blend (FuncAdd,FuncAdd) (b,(SrcAlpha,OneMinusSrcAlpha)) (RGBA (0:.0:.0:.()) 1)
    texFun = case saTexture sa of
        ST_Map t        -> \_ _ -> loadQ3Texture mipmap $ SB.unpack t
        ST_ClampMap t   -> \_ _ -> loadQ3Texture mipmap $ SB.unpack t
        ST_AnimMap f l  -> \_ t -> let
            txl = map (loadQ3Texture mipmap . SB.unpack) l
            i = floor $ (fromIntegral $ length l) * fract' (t*f)
            in txl !! i
        ST_Lightmap     -> \lm _ -> lm
        ST_WhiteImage   -> \_ _ -> whiteImage
    sampleFun = case saTexture sa of
        ST_ClampMap _   -> \t uv -> sample (Sampler Linear Clamp) t uv
        ST_WhiteImage   -> \_ _ -> RGBA (1:.1:.1:.()) 1
        _               -> \t uv -> sample (Sampler Linear Wrap) t uv

stageRenderer :: ComparisonFunction -> Bool -> Blending -> VertexDeformer -> RGBFun -> AlphaFun -> TCFun -> TexFun -> SampleFun -> Renderer
stageRenderer depthFun depthWrite blending vertexFun rgbFun alphaFun tcFun texFun sampleFun lmTex obj time' time cWorldProjection fb =
    paintColorRastDepth depthFun depthWrite blending (RGBA (Vec.vec True) True) (rast obj) fb
  where
    rast obj = fmap frag $ rasterizeBack $ fmap vert obj
    vert vd@(v3,_,_,_) = (cWorldProjection `multmv` v4,(rgbFun time vd, alphaFun time vd, tcFun time vd))
      where
        v4 = Vec.snoc (vertexFun time v3) 1
    frag (rgb,a,uv) = RGBA (rgb * rgb') (a * a')
      where
        RGBA rgb' a' = sampleFun (texFun lmTex time') uv

stagesRenderer :: CommonAttrs -> (Int,Renderer)
stagesRenderer ca = (caSort ca, \lm obj time' time cWorldProjection fb -> foldl' (\f r -> r lm obj time' time cWorldProjection f) fb $ reverse $ caRenderers ca)

renderSurfaces :: Float -> Vertex Float -> Vec.Mat44 (Vertex Float) -> V.Vector (Int,SurfaceRenderer) -> FB
renderSurfaces time' time worldProjection faces = V.foldl' (foldl' (\fb fun -> fun time' time worldProjection fb)) cleanFB $ sorted
  where
    maxSort = 256
    cleanFB = newFrameBufferColorDepth (RGBA (0:.0:.0:.()) 1) 1000
    sorted  = V.accumulate (\l e -> e:l) (V.replicate maxSort []) faces

{-
#define LIGHTMAP_2D			-4		// shader is for 2D rendering
#define LIGHTMAP_BY_VERTEX	-3		// pre-lit triangle models
#define LIGHTMAP_WHITEIMAGE	-2
#define	LIGHTMAP_NONE		-1
-}
imageRenderer lmidx txName = stagesRenderer $ if lmidx < 0 then ca else addRenderer ca saLM
  where
    ca = addRenderer defaultCommonAttrs sa
    sa = defaultStageAttrs
        { saTexture = ST_Map txName
--        , saBlend = Just (SrcColor,Zero)
--        , saBlend = Just (SrcColor,DstColor)
        }
    saLM = defaultStageAttrs
        { saTexture = ST_Lightmap
        , saTCGen = \(_,_,uv,_) -> uv
--        , saBlend = Just (SrcColor,One)
        , saBlend = Just (SrcColor,DstColor)
        }

compileBSP :: Trie (Int,Renderer) -> BSPLevel -> V.Vector (Int,SurfaceRenderer)
compileBSP shaderMap bsp = V.map convertSurface $ blSurfaces bsp
  where
    lightmaps = V.map (textureFromByteString True 3 128 128 . lmMap) $ blLightmaps bsp
    shaders = V.map (\s -> T.lookup (shName s) shaderMap) $ blShaders bsp
    convertSurface sf = (shidx,sh (lightmap $ srLightmapNum sf) geom)
      where
        shaderName = shName $ (blShaders bsp) V.! (srShaderNum sf)
        (shidx,sh) = case shaders V.! srShaderNum sf of
            Just s  -> s
            Nothing -> imageRenderer (srLightmapNum sf) shaderName
        geom = case srSurfaceType sf of
            Planar       -> toIndexedGPUStream TriangleList v i
            TriangleSoup -> toIndexedGPUStream TriangleList v i
            Patch        -> toGPUStream TriangleList $ concatMap (pointToCube (0:.1:.0:.1:.())) v
            Flare        -> toGPUStream TriangleList $ concatMap (pointToCube (1:.0:.0:.1:.())) v
            --_            -> toGPUStream TriangleList $ concatMap (pointToCube (1:.1:.0:.1:.())) v
        v = V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) vertices
        i = V.toList $ V.take (srNumIndices sf) $ V.drop (srFirstIndex sf) indices
        lightmap lidx | 0 <= lidx && lidx < V.length lightmaps = lightmaps V.! lidx
                      | otherwise = whiteImage

    vertices = V.map convertVertex $ blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p,v2 dt,v2 lt,v4 c)
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = i:.j:.k:.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()

isClusterVisible :: BSPLevel -> Int -> Int -> Bool
isClusterVisible bl a b
    | a >= 0 = 0 /= (visSet .&. (shiftL 1 (b .&. 7)))
    | otherwise = True
  where
    Visibility nvecs szvecs vecs = blVisibility bl
    i = a * szvecs + (shiftR b 3)
    visSet = vecs V.! i

findLeafIdx bl camPos i
    | i >= 0 = if dist >= 0 then findLeafIdx bl camPos f else findLeafIdx bl camPos b
    | otherwise = (-i) - 1
  where 
    node    = blNodes bl V.! i
    (f,b)   = ndChildren node 
    plane   = blPlanes bl V.! ndPlaneNum node
    dist    = plNormal plane `Vect.dotprod` camPos - plDist plane

data Frustum
    = Frustum
    { frPlanes :: Vec.Vec6 Plane
    , ntl :: Vect.Vec3
    , ntr :: Vect.Vec3
    , nbl :: Vect.Vec3
    , nbr :: Vect.Vec3
    , ftl :: Vect.Vec3
    , ftr :: Vect.Vec3
    , fbl :: Vect.Vec3
    , fbr :: Vect.Vec3
    }

pointInFrustum p fr = Vec.foldr (\(Plane n d) b -> b && d + n `Vect.dotprod` p >= 0) True $ frPlanes fr

sphereInFrustum p r fr = Vec.foldr (\(Plane n d) b -> b && d + n `Vect.dotprod` p >= (-r)) True $ frPlanes fr

boxInFrustum pp pn fr = Vec.foldr (\(Plane n d) b -> b && d + n `Vect.dotprod` (g pp pn n) >= 0) True $ frPlanes fr
  where
    g (Vect.Vec3 px py pz) (Vect.Vec3 nx ny nz) n = Vect.Vec3 (fx px nx) (fy py ny) (fz pz nz)
      where
        Vect.Vec3 x y z = n
        fx:.fy:.fz:.() = Vec.map (\a -> if a > 0 then max else min) (x:.y:.z:.())

cullSurfaces bsp cam frust surfaces = case leafIdx < 0 || leafIdx >= V.length leaves of
    True    -> unsafePerformIO $ print ("findLeafIdx error") >> return surfaces
    False   -> unsafePerformIO $ print ("findLeafIdx ok",leafIdx,camCluster) >> return (V.ifilter (\i _ -> surfaceMask V.! i) surfaces)
  where
    leafIdx = findLeafIdx bsp cam 0
    leaves = blLeaves bsp
    camCluster = lfCluster $ leaves V.! leafIdx
    visibleLeafs = V.filter (\a -> (isClusterVisible bsp camCluster $ lfCluster a) && inFrustum a) leaves
    surfaceMask = unsafePerformIO $ do
        let leafSurfaces = blLeafSurfaces bsp
        mask <- MV.replicate (V.length surfaces) False
        V.forM_ visibleLeafs $ \l ->
            V.forM_ (V.slice (lfFirstLeafSurface l) (lfNumLeafSurfaces l) leafSurfaces) $ \i ->
                MV.write mask i True
        V.unsafeFreeze mask
    inFrustum a = boxInFrustum (lfMaxs a) (lfMins a) frust

frustum :: Float -> Float -> Float -> Float -> Vect.Vec3 -> Vect.Vec3 -> Vect.Vec3 -> Frustum
frustum angle ratio nearD farD p l u = Frustum ((pl ntr ntl ftl):.(pl nbl nbr fbr):.(pl ntl nbl fbl):.
                                                (pl nbr ntr fbr):.(pl ntl ntr nbr):.(pl ftr ftl fbl):.()) ntl ntr nbl nbr ftl ftr fbl fbr
  where
    pl a b c = Plane n d
      where
        n = Vect.normalize $ (c - b) `Vect.crossprod` (a - b)
        d = -(n `Vect.dotprod` b)
    m a v = Vect.scalarMul a v
    ang2rad = pi / 180
    tang    = tan $ angle * ang2rad * 0.5
    nh  = nearD * tang
    nw  = nh * ratio
    fh  = farD * tang
    fw  = fh * ratio
    z   = Vect.normalize $ p - l
    x   = Vect.normalize $ u `Vect.crossprod` z
    y   = z `Vect.crossprod` x

    nc  = p - m nearD z
    fc  = p - m farD z

    ntl = nc + m nh y - m nw x
    ntr = nc + m nh y + m nw x
    nbl = nc - m nh y - m nw x
    nbr = nc - m nh y + m nw x

    ftl = fc + m fh y - m fw x
    ftr = fc + m fh y + m fw x
    fbl = fc - m fh y - m fw x
    fbr = fc - m fh y + m fw x
