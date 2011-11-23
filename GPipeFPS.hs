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
type Renderer = Mesh -> SurfaceRenderer

type RGBFun    = Vertex Float -> VertexData -> Vec.Vec3 (Vertex Float)
type AlphaFun  = Vertex Float -> VertexData -> Vertex Float
type TCFun     = Vertex Float -> VertexData -> Vec.Vec2 (Vertex Float)
type TexFun    = Float -> Texture2D RGBAFormat
type SampleFun = Texture2D RGBAFormat -> Vec.Vec2 (Fragment Float) -> Color RGBAFormat (Fragment Float)

type VertexDeformer = Vertex Float -> Vec.Vec3 (Vertex Float) -> Vec.Vec3 (Vertex Float)

data LightmapSource
    = LS_2D
    | LS_ByVertex
    | LS_WhiteImage
    | LS_None
    | LS_Texture (Texture2D RGBAFormat)

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
    , saRGBGen      :: () -- texture, vertex color, constant
    , saAlphaGen    :: () -- texture, vertex color, constant
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
    , saRGBGen      = ()
    , saAlphaGen    = ()
    , saTCGen       = \(_,uv,_,_) -> uv
    , saTCMod       = []
    , saTexture     = ST_WhiteImage
    , saDepthWrite  = False
    , saDepthFunc   = Lequal
    , saAlphaFunc   = ()
    }

addRenderer ca sa = ca {caRenderers = r:caRenderers ca}
  where
    r = stageRenderer (saDepthFunc sa) depthWrite blend vertexFun rgbFun alphaFun tcFun texFun sampleFun whiteImage
    vertexFun t v = v
    rgbFun t (_,_,_,r:.g:.b:._:.()) = r:.g:.b:.()
    alphaFun t (_,_,_,_:._:._:.a:.()) = a
    tcFun t vd = foldl' (\uv f -> f t uv) (saTCGen sa vd) (reverse $ saTCMod sa)
    depthWrite = if NoBlending == blend then True else True --saDepthWrite sa
    blend = case saBlend sa of
        Nothing -> NoBlending
        Just b  -> Blend (FuncAdd,FuncAdd) (b,(SrcAlpha,OneMinusSrcAlpha)) (RGBA (0:.0:.0:.()) 1)
    texFun = case saTexture sa of
        ST_Map t        -> \_ -> loadQ3Texture $ SB.unpack t
        ST_ClampMap t   -> \_ -> loadQ3Texture $ SB.unpack t
        ST_AnimMap f l  -> \t -> let
            txl = map (loadQ3Texture . SB.unpack) l
            i = floor $ (fromIntegral $ length l) * fract' (t*f)
            in txl !! i
        ST_Lightmap     -> \_ -> whiteImage -- TODO
        ST_WhiteImage   -> \_ -> whiteImage
    sampleFun = case saTexture sa of
        ST_ClampMap _   -> \t uv -> sample (Sampler Linear Clamp) t uv
        ST_WhiteImage   -> \_ _ -> RGBA (1:.1:.1:.()) 1
        _               -> \t uv -> sample (Sampler Linear Wrap) t uv

mkRenderer tx = r
  where
    r = stageRenderer Lequal True NoBlending vertexFun rgbFun alphaFun tcFun texFun sampleFun whiteImage
    vertexFun t v = v
    rgbFun t (_,_,_,r:.g:.b:._:.()) = r:.g:.b:.()
    alphaFun t (_,_,_,_:._:._:.a:.()) = a
    tcFun t (_,uv,_,_) = uv
    sampleFun t uv = sample (Sampler Linear Wrap) t uv
    texFun t = tx

stageRenderer :: ComparisonFunction -> Bool -> Blending -> VertexDeformer -> RGBFun -> AlphaFun -> TCFun -> TexFun -> SampleFun -> Texture2D RGBAFormat -> Renderer
stageRenderer depthFun depthWrite blending vertexFun rgbFun alphaFun tcFun texFun sampleFun tex obj time' time cWorldProjection fb =
    paintColorRastDepth depthFun depthWrite blending (RGBA (Vec.vec True) True) (rast obj) fb
  where
    rast obj = fmap frag $ rasterizeBack $ fmap vert obj
    vert vd@(v3,_,_,_) = (cWorldProjection `multmv` v4,(rgbFun time vd, alphaFun time vd, tcFun time vd))
      where
        v4 = Vec.snoc (vertexFun time v3) 1
    frag (rgb,a,uv) = RGBA (rgb * rgb') (a * a')
      where
        RGBA rgb' a' = sampleFun (texFun time') uv

stagesRenderer :: CommonAttrs -> (Int,Renderer)
stagesRenderer ca = (caSort ca, \obj time' time cWorldProjection fb -> foldl' (\f r -> r obj time' time cWorldProjection f) fb $ reverse $ caRenderers ca)

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
{-
additional parameters to renderer:
    - lightmap index
    - lightmap vector
    - entity color
-}

compileBSP :: Trie (Int,Renderer) -> BSPLevel -> V.Vector (Int,SurfaceRenderer)
compileBSP shaderMap bsp = V.map convertSurface $ blSurfaces bsp
  where
    lookup k = case T.lookup (shName k) shaderMap of
        Nothing -> (3,mkRenderer $ loadQ3Texture $ SB.unpack $ shName k)
        Just v  -> v
    shaders = V.map lookup $ blShaders bsp
    --lightmaps = V.map (textureFromByteString 3 128 128 . lmMap) $ blLightmaps bsp

    convertSurface sf = (shidx,sh geom)
      where
        (shidx,sh) = shaders V.! srShaderNum sf
        geom = case srSurfaceType sf of
            Planar       -> toIndexedGPUStream TriangleList v i
            TriangleSoup -> toIndexedGPUStream TriangleList v i
            --Patch        -> toIndexedGPUStream TriangleStrip v $ grid $ srPatchSize sf
            --Flare        -> toGPUStream Point v
            _              -> toGPUStream TriangleList []
        v = V.toList $ V.take (srNumVertices sf) $ V.drop (srFirstVertex sf) vertices
        i = V.toList $ V.take (srNumIndices sf) $ V.drop (srFirstIndex sf) indices
        grid (w,h) = unsafePerformIO $ print ("patch",w*h,srNumVertices sf) >> return (even 0 0)
          where
            {- 
               hint: http://dan.lecocq.us/wordpress/wp-content/uploads/2009/12/strip.png
               a - b
               |   |
               c - d
            -}
            even x y
                | x >= w-1  = c:odd (w-2) (y+1)
                | y >= h    = []
                | otherwise = a:c:b:d:even (x+1) y
              where
                a = y*w+x
                b = y*w+x+1
                c = (y+1)*w+x
                d = (y+1)*w+x+1
            odd x y
                | x < 0     = d:even 0 (y+1)
                | y >= h    = []
                | otherwise = b:d:a:c:odd (x-1) y
              where
                a = y*w+x
                b = y*w+x+1
                c = (y+1)*w+x
                d = (y+1)*w+x+1

        --lm = if 0 <= lmidx && lmidx < V.length lightmaps then Just $ lightmaps V.! lmidx else Nothing
        --lmidx = srLightmapNum sf

    vertices = V.map convertVertex $ blDrawVertices bsp
    indices  = blDrawIndices bsp
    convertVertex (DrawVertex p dt lt n c) = (v3 p,v2 dt,v2 lt,v4 c)
    v2 (Vect.Vec2 i j) = i:.j:.()
    v3 (Vect.Vec3 i j k) = (s i):.(s j):.(s k):.()
    v3' (Vect.Vec3 i j k) = i:.j:.k:.()
    v4 (Vect.Vec4 i j k l) = i:.j:.k:.l:.()
    --s a = 0.01 * a
    s a = a

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

{-
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
-}
