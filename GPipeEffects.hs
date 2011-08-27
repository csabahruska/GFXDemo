module GPipeEffects where

import Graphics.GPipe
import Data.List
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Vec.LinAlg.Transform3D

simple cWorldProjection objs = foldl' (\fb obj -> paintColorDepth Less True NoBlending (RGB $ Vec.vec True) (rast obj) fb) clear objs
  where
    clear = newFrameBufferColorDepth (RGB (0:.0:.0:.())) 1000

    --vert :: Vec3 (Vertex Float) -> (Vec4 (Vertex Float),Vec4 (Vertex Float))
    vert v = (cWorldProjection `multmv` v4,cWorldProjection `multmv` v4)
      where
        v4 = Vec.snoc v 1

    --rast :: PrimitiveStream Triangle (Vec3 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
    rast obj = fmap frag $ rasterizeFront $ fmap vert obj

    --frag :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
    frag (x:.y:.z:.w:.()) = (RGB ((fract' x):.(fract' y):.(1 + fract' z):.()),z / w)

----------
-- VSM ---
----------

--moments :: Vec.Mat44 (Vertex Float) -> [PrimitiveStream Triangle (Vec3 (Vertex Float))] -> FrameBuffer RGBFormat () ()
moments worldProjection objs = foldl' (\fb obj -> paintColorDepth Less True NoBlending (RGB $ Vec.vec True) (rast obj) fb) clear objs
  where
    clear = newFrameBufferColorDepth (RGB (0:.0:.0:.())) 100

    --rast :: PrimitiveStream Triangle (Vec3 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
    rast obj = fmap storeDepth $ rasterizeFront $ fmap vert obj

    --vert :: Vec.Vec3 (Vertex Float) -> (Vec.Vec4 (Vertex Float),Vertex Float)
    vert v = (v4,v4)
      where
        v4 = worldProjection `multmv` (Vec.snoc v 1)

    --storeDepth :: Fragment Float -> Color RGBFormat (Fragment Float)
    storeDepth (x:.y:.depth':.w:.()) = (RGB (moment1:.moment2:.0:.()),depth'/w)
      where
        depth = depth'--  * 0.5 + 0.5
        dx = dFdx depth
        dy = dFdy depth
        moment1 = depth
        moment2 = depth*depth + 0.25 * (dx*dx + dy*dy)

--vsm :: Vec.Mat44 (Vertex Float) -> Vec.Mat44 (Vertex Float) -> [PrimitiveStream Triangle (Vec3 (Vertex Float))] -> FrameBuffer RGBFormat () ()
vsm cWorldProjection lWorldProjection objs = foldl' (\fb obj -> paintColorDepth Less True NoBlending (RGB $ Vec.vec True) (rast obj) fb) clear objs
  where
    clear = newFrameBufferColorDepth (RGB (0:.0:.0:.())) 100

    shadowTexture :: Texture2D RGBFormat
    shadowTexture = fromFrameBufferColor RGB32F (512:.512:.()) $ moments lWorldProjection objs

    --vert :: Vec3 (Vertex Float) -> (Vec4 (Vertex Float),Vec4 (Vertex Float))
    vert v = (p,(p,lWorldProjection `multmv` v4))
      where
        p = cWorldProjection `multmv` v4
        v4 = Vec.snoc v 1

    --rast :: PrimitiveStream Triangle (Vec3 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
    rast obj = fmap calcLuminance $ rasterizeFront $ fmap vert obj

    --calcLuminance :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
    calcLuminance (x:.y:.z:.w:.(),tx:.ty:.tz:.tw:.()) = (RGB {-$ if tz <= m1 then 1:.1:.1:.() else-} ((fn x):.(fn y):.(fn z):.()),z/w)
      where
        RGB (m1:.m2:._) = sample (Sampler Linear Clamp) shadowTexture ((tx/tw*0.5+0.5):.(ty/tw*0.5+0.5):.())
        variance = max 0.002 (m2 - m1*m1)
        d = tz - m1
        p_max = variance / (variance + d*d)

        fn a = (p_max + 0.1) * fract' a
