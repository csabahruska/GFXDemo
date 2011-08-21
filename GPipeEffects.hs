module GPipeEffects where

import Graphics.GPipe
import Data.List
import qualified Data.Vec as Vec
import Data.Vec.Nat
import Data.Vec.LinAlg.Transform3D

-- VSM
moments :: Vec.Mat44 (Vertex Float) -> [PrimitiveStream Triangle (Vec3 (Vertex Float))] -> FrameBuffer RGBFormat () ()
moments worldProjection objs = foldl' (\fb obj -> paintColor NoBlending (RGB $ Vec.vec True) (rast obj) fb) clear objs
  where
    clear = newFrameBufferColor (RGB (0:.0:.0:.()))

    rast :: PrimitiveStream Triangle (Vec3 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
    rast obj = fmap storeDepth $ rasterizeBack $ fmap vert obj

    vert :: Vec.Vec3 (Vertex Float) -> (Vec.Vec4 (Vertex Float),Vertex Float)
    vert v = (v4,Vec.get Vec.n2 v4)
      where
        v4 = worldProjection `multmv` (Vec.snoc v 1)

    storeDepth :: Fragment Float -> Color RGBFormat (Fragment Float)
    storeDepth depth = RGB (moment1:.moment2:.0:.())
      where
        dx = dFdx depth
        dy = dFdy depth
        moment1 = depth
        moment2 = depth*depth + 0.25 * (dx*dx + dy*dy)

vsm :: Vec.Mat44 (Vertex Float) -> Vec.Mat44 (Vertex Float) -> [PrimitiveStream Triangle (Vec3 (Vertex Float))] -> FrameBuffer RGBFormat () ()
vsm cWorldProjection lWorldProjection objs = foldl' (\fb obj -> paintColor NoBlending (RGB $ Vec.vec True) (rast obj) fb) clear objs
  where
    clear = newFrameBufferColor (RGB (0:.0:.0:.()))

    shadowTexture :: Texture2D RGBFormat
    shadowTexture = fromFrameBufferColor RGB8 (512:.512:.()) $ moments lWorldProjection objs

    vert :: Vec3 (Vertex Float) -> (Vec4 (Vertex Float),Vec4 (Vertex Float))
    vert v = (cWorldProjection `multmv` v4,lWorldProjection `multmv` v4)
      where
        v4 = Vec.snoc v 1

    rast :: PrimitiveStream Triangle (Vec3 (Vertex Float)) -> FragmentStream (Color RGBFormat (Fragment Float))
    rast obj = fmap calcLuminance $ rasterizeBack $ fmap vert obj

    calcLuminance :: Vec4 (Fragment Float) -> Color RGBFormat (Fragment Float)
    calcLuminance (tx:.ty:.tz:.tw:.()) = RGB {-$ if tz <= m1 then 1:.1:.1:.() else-} (c:.c:.c:.())
      where
        RGB (m1:.m2:._) = sample (Sampler Linear Clamp) shadowTexture ((tx/tw):.(ty/tw):.())
        variance = max 0.002 (m2 - m1*m1)
        d = tz - m1
        p_max = variance / (variance + d*d)
        c = 0.1 + p_max
