{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Monoid
import Data.Vec.LinAlg.Transform3D
import Data.Vec.Nat
import FRP.Elerea.Param
import GPipeFPS
import GPipeUtils
import qualified BSPLoader as B
import ShaderParser
import Utils as U
import Graphics.GPipe
import qualified Data.Map as Map
import qualified Data.Vec as Vec
import qualified Data.Vect.Float as V
import Graphics.Rendering.OpenGL ( Position(..) )
import Graphics.UI.GLUT( Window,
                         mainLoop,
                         postRedisplay,
                         idleCallback,
                         passiveMotionCallback,
                         getArgsAndInitialize,
                         ($=),
                         KeyboardMouseCallback,
                         Key(..),
                         KeyState(..),
                         SpecialKey(..),
                         keyboardMouseCallback)


main :: IO ()
main = do
    getArgsAndInitialize

    -- setup FRP environment
    (winSize,winSizeSink) <- external (0,0)
    (mousePosition,mousePositionSink) <- external (0,0)
    (buttonPress,buttonPressSink) <- external False
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    bsp <- B.loadBSP "fps/maps/pukka3tourney7.bsp"
    obj1 <- loadGPipeMesh "Monkey.lcmesh"
    obj2 <- loadGPipeMesh "Scene.lcmesh"
    obj3 <- loadGPipeMesh "Plane.lcmesh"
    obj4 <- loadGPipeMesh "Icosphere.lcmesh"

    net <- start $ scene bsp mousePosition fblrPress buttonPress winSize
    keys <- newIORef $ Map.empty


    putStrLn "creating window..."
    newWindow "FPS Demo" 
        (100:.100:.()) 
        (800:.600:.()) 
        (renderFrame keys fblrPressSink buttonPressSink winSizeSink net)
        (initWindow keys mousePositionSink)
    putStrLn "entering mainloop..."
    mainLoop

--renderFrame :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame keys fblrPress buttonPress winSize net (w:.h:.()) = do
    km <- readIORef keys
    let keyIsPressed c = case Map.lookup c km of
            Nothing -> False
            Just v -> v

    winSize (w,h)
    {-
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)
    -}
    fblrPress (keyIsPressed $ SpecialKey KeyLeft, keyIsPressed $ SpecialKey KeyUp, keyIsPressed $ SpecialKey KeyDown, keyIsPressed $ SpecialKey KeyRight, False)
    buttonPress $ keyIsPressed $ Char ' '

    --tmp <- keyIsPressed KeySpace
    --print (x,y,tmp)
    --updateFPS s t
    let t = 0.03
    join $ net $ realToFrac t

--initWindow :: Window -> IO ()
initWindow keys mousePositionSink win = do
    keyboardMouseCallback $= Just (keyboard keys mousePositionSink)
    passiveMotionCallback $= Just (\(Position x y) -> mousePositionSink (fromIntegral x,fromIntegral y))
    idleCallback $= Just (postRedisplay (Just win))

{-
scene :: PrimitiveStream Triangle (Vec3 (Vertex Float))
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal (Bool)
      -> Signal (Int, Int)
      -> SignalGen Float (Signal (IO (FrameBuffer RGBFormat () ())))
-}
scene bsp mousePosition fblrPress buttonPress wh = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    --let mouseMove = mousePosition
    cam <- userCamera (V.Vec3 (-4) 0 0) mouseMove fblrPress
    return $ drawGLScene bsp <$> wh <*> cam <*> time <*> buttonPress

convMat :: V.Mat4 -> Vec.Mat44 (Vertex Float)
convMat m = toGPU $ (v a):.(v b):.(v c):.(v d):.()
  where
    V.Mat4 a b c d = V.transpose m
    v (V.Vec4 x y z w) = x:.y:.z:.w:.()

{-
drawGLScene :: PrimitiveStream Triangle (Vec3 (Vertex Float))
            -> (Int,Int)
            -> (V.Vec3, V.Vec3, V.Vec3, t1)
            -> Float
            -> Bool
            -> IO (FrameBuffer RGBFormat () ())
-}
drawGLScene bsp (w,h) (cam,dir,up,_) time buttonPress = do
    let cm = V.fromProjective (lookat cam (cam + dir) up)
        pm = U.perspective 0.1 50 90 (fromIntegral w / fromIntegral h)

        lpos = V.Vec3 0.1 2 0.1
        lat  = (V.Vec3 0 (-100) 0)
        lup  = V.Vec3 0 1 0
        lmat = V.fromProjective (lookat lpos lat lup)
        pmat = U.perspective 0.4 5 90 (fromIntegral w / fromIntegral h)
    return $ renderBSP (convMat (cm V..*. pm)) bsp
    --return $ vsm (convMat (cm V..*. pm)) (convMat (cm V..*. pm)) objs
    --return $ moments (convMat (cm V..*. pmat)) objs
    --return $ simple (convMat (cm V..*. pm)) objs

-- Key -> KeyState -> Modifiers -> Position -> IO ()
keyboard keys mousePos key keyState mods (Position x y) = do
    mousePos (fromIntegral x,fromIntegral y)
    modifyIORef keys $ \m -> case (key, keyState) of
        (c, Down) -> Map.insert c True m
        (c, Up) -> Map.insert c False m
