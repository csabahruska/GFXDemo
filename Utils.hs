{-# LANGUAGE OverloadedStrings, PackageImports #-}
module Utils where

import Control.Applicative
import Control.Monad
import Data.IORef

import Data.Bits
--import Data.ByteString.Char8 (ByteString)
import Data.Vect
import Data.Vect.Float.Instances ()
import FRP.Elerea.Param
import "GLFW-b" Graphics.UI.GLFW as GLFW
import GraphicsPipeline
import qualified Data.Vector.Storable as V

-- Reactive helper functions

integral :: (Real p, Fractional t) => t -> Signal t -> SignalGen p (Signal t)
integral v0 s = transfer v0 (\dt v v0 -> v0+v*realToFrac dt) s

driveNetwork :: (p -> IO (IO a)) -> IO (Maybe p) -> IO ()
driveNetwork network driver = do
    dt <- driver
    case dt of
        Just dt -> do
            join $ network dt
            driveNetwork network driver
        Nothing -> return ()

-- OpenGL/GLFW boilerplate

initCommon :: String -> IO (Signal (Int, Int))
initCommon title = do
    initialize
    openWindow defaultDisplayOptions
        { displayOptions_numRedBits     = 8
        , displayOptions_numGreenBits   = 8
        , displayOptions_numBlueBits    = 8
        , displayOptions_numDepthBits   = 24
        , displayOptions_width          = 1280
        , displayOptions_height         = 720
--        , displayOptions_displayMode    = Fullscreen
        }
    setWindowTitle title

    (windowSize,windowSizeSink) <- external (0,0)
    setWindowSizeCallback $ \w h -> windowSizeSink (fromIntegral w, fromIntegral h)

    return windowSize

-- FPS tracking

data State = State { frames :: IORef Int, t0 :: IORef Double }

fpsState :: IO State
fpsState = State <$> newIORef 0 <*> newIORef 0

updateFPS :: State -> Double -> IO ()
updateFPS state t1 = do
    let t = 1000*t1
        fR = frames state
        tR = t0 state
    modifyIORef fR (+1)
    t0' <- readIORef tR
    writeIORef tR $ t0' + t
    when (t + t0' >= 5000) $ do
    f <- readIORef fR
    let seconds = (t + t0') / 1000
        fps = fromIntegral f / seconds
    putStrLn (show f ++ " frames in " ++ show seconds ++ " seconds = "++ show fps ++ " FPS")
    writeIORef tR 0
    writeIORef fR 0

-- Continuous camera state (rotated with mouse, moved with arrows)
userCamera :: Real p => Vec3 -> Signal (Float, Float) -> Signal (Bool, Bool, Bool, Bool, Bool)
           -> SignalGen p (Signal (Vec3, Vec3, Vec3, (Float, Float)))
userCamera p mposs keyss = transfer2 (p,zero,zero,(0,0)) calcCam mposs keyss
  where
    d0 = Vec4 0 0 (-1) 1
    u0 = Vec4 0 1 0 1
    calcCam dt (dmx,dmy) (ka,kw,ks,kd,turbo) (p0,_,_,(mx,my)) = (p',d,u,(mx',my'))
      where
        f0 c n = if c then (&+ n) else id
        p'  = foldr1 (.) [f0 ka (v &* (-t)),f0 kw (d &* t),f0 ks (d &* (-t)),f0 kd (v &* t)] p0
        k   = if turbo then 100 else 30
        t   = k * realToFrac dt
        mx' = dmx + mx
        my' = dmy + my
        rm  = fromProjective $ rotationEuler $ Vec3 (mx' / 100) (my' / 100) 0
        d   = trim $ rm *. d0 :: Vec3
        u   = trim $ rm *. u0 :: Vec3
        v   = normalize $ d &^ u

followCamera :: Float -> Float -> Float -> Signal Proj4 -> SignalGen p (Signal (Vec3, Vec3))
followCamera height minDist maxDist target = transfer (Vec3 (-maxDist) height 0, Vec3 1 0 0) follow target
  where
    follow _dt tproj (pos,_dir) = (pos',tpos &- pos')
      where
        Mat4 _ _ _ tpos4 = fromProjective (tproj .*. translation (Vec3 0 height 0))
        tpos = trim tpos4
        tdir = tpos &- pos
        dist = len tdir
        pos'
            | dist < minDist = pos &+ (normalize tdir &* (dist-minDist))
            | dist > maxDist = pos &+ (normalize tdir &* (dist-maxDist))
            | otherwise      = pos

-- | Perspective transformation matrix.
perspective :: Float  -- ^ Near plane clipping distance (always positive).
            -> Float  -- ^ Far plane clipping distance (always positive).
            -> Float  -- ^ Field of view of the y axis, in radians.
            -> Float  -- ^ Aspect ratio, i.e. screen's width\/height.
            -> Mat4
perspective n f fovy aspect = transpose $
    Mat4 (Vec4 (2*n/(r-l)) 0 (-(r+l)/(r-l)) 0)
         (Vec4 0 (2*n/(t-b)) ((t+b)/(t-b)) 0)
         (Vec4 0 0 (-(f+n)/(f-n)) (-2*f*n/(f-n)))
         (Vec4 0 0 (-1) 0)
  where
    t = n*tan(fovy/2)
    b = -t
    r = aspect*t
    l = -r

-- | Pure orientation matrix defined by Euler angles.
rotationEuler :: Vec3 -> Proj4
rotationEuler (Vec3 a b c) = orthogonal $ toOrthoUnsafe $ rotMatrixY a .*. rotMatrixX b .*. rotMatrixZ c

-- | Camera transformation matrix.
lookat :: Vec3   -- ^ Camera position.
       -> Vec3   -- ^ Target position.
       -> Vec3   -- ^ Upward direction.
       -> Proj4
lookat pos target up = translateBefore4 (neg pos) (orthogonal $ toOrthoUnsafe r)
  where
    w = normalize $ pos &- target
    u = normalize $ up &^ w
    v = w &^ u
    r = transpose $ Mat3 u v w

addNormal :: Mesh -> Mesh
addNormal (Mesh [("position",A_Vec3 p)] (TrianglesI idx)) = Mesh [("position",A_Vec3 p'),("normal",A_Vec3 n)] $ TrianglesI idx'
  where
    p'  = V.backpermute p idx
    pv i = p' V.! i -- p V.! (idx V.! i)
    nv i = normalize $ (b-a) &^ (c-a)
      where
        a = pv i
        b = pv $ i + 1
        c = pv $ i + 2
    n   = V.concatMap (V.replicate 3 . nv) $ V.enumFromStepN 0 3 (V.length idx `div` 3)
    idx'= V.enumFromN 0 (V.length p')

addNormal _ = error "Unsupported mesh format"

cube :: Mesh
cube = addNormal cube'

cube' :: Mesh
cube' = Mesh [("position", pos)] $ TrianglesI idx
  where
    quads = [[6,2,3,7]
            ,[5,1,0,4]
            ,[7,3,1,5]
            ,[4,0,2,6]
            ,[3,2,0,1]
            ,[6,7,5,4]]
    mkVertex :: Int -> Vec3
    mkVertex n = Vec3 x y z
      where
        x = if testBit n 2 then 1 else -1
        y = if testBit n 1 then 1 else -1
        z = if testBit n 0 then 1 else -1
    pos = A_Vec3 $ V.fromList [mkVertex i | i <- [0..7]]
    idx = V.fromList $ concat [[a,b,c,c,d,a] | [a,b,c,d] <- quads]

plane :: Mesh
plane = addNormal plane'

plane' :: Mesh
plane' = Mesh [("position", pos)] $ TrianglesI $ V.fromList [0,1,2,2,3,0]
  where
    pos = A_Vec3 $ V.fromList [Vec3 n 0 p, Vec3 n 0 n, Vec3 p 0 n, Vec3 p 0 p]
    p = 1
    n = -1

quad2D :: Mesh
quad2D = Mesh [("position", pos)] $ TrianglesI $ V.fromList [0,1,2,2,3,0]
  where
    pos = A_Vec2 $ V.fromList [Vec2 n n, Vec2 n p, Vec2 p p, Vec2 p n]
    p = 1
    n = -1
