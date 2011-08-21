{-# LANGUAGE OverloadedStrings, PackageImports #-}
import "GLFW-b" Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.List
import Data.Monoid
import Data.Vec.LinAlg.Transform3D
import Data.Vec.Nat
import FRP.Elerea.Param
import GPipeEffects
import GPipeUtils
import Graphics.GPipe
import qualified Data.Vec as Vec
import Graphics.UI.GLUT( Window,
                         mainLoop,
                         postRedisplay,
                         idleCallback,
                         getArgsAndInitialize,
                         ($=),
                         KeyboardMouseCallback,
                         Key(..),
                         KeyState(..),
                         keyboardMouseCallback)

main :: IO ()
main = do
    getArgsAndInitialize
    initialize
    putStrLn "creating window..."
    scene <- loadGPipeMesh "Monkey.lcmesh"
    newWindow "Green Triangle" 
        (100:.100:.()) 
        (800:.600:.()) 
        (renderFrame scene)
        initWindow
    putStrLn "entering mainloop..."
    mainLoop

--renderFrame :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame scene _ = do
    let cm = scaling (1:.1:.1:.())
        lm = scaling (1:.1:.1:.())
    return $ vsm cm lm [scene]

initWindow :: Window -> IO ()
initWindow win = do
    (mousePosition,mousePositionSink) <- external (0,0)
    (_mousePress,mousePressSink) <- external False
    (buttonPress,buttonPressSink) <- external False
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    idleCallback $= Nothing --Just (postRedisplay (Just win))
    keyboardMouseCallback $= Just (keyboard mousePositionSink mousePressSink fblrPressSink buttonPressSink)
{-
    sc <- start $ scene ssao vsm bloom fb renderTex diffuseTex glProgram scn windowSize mousePosition fblrPress buttonPress
    driveNetwork sc (readInput s mousePositionSink mousePressSink fblrPressSink buttonPressSink)

    --displayCallback $= display state
    --reshapeCallback $= Just reshape
    --addTimerCallback timerFrequencyMillis (timer state)

scene :: SSAO -> VSM -> Bloom -> GLFramebuffer -> GLColorTexture2D -> GLColorTexture2D -> GLProgram -> Scene -> Signal (Int, Int)
      -> Signal (Float, Float)
      -> Signal (Bool, Bool, Bool, Bool, Bool)
      -> Signal (Bool)
      -> SignalGen Float (Signal (IO ()))
scene ssao vsm bloom fb rtex tex p scn windowSize mousePosition fblrPress buttonPress = do
    re1 <- integral 0 1.5
    re2 <- integral 10 (-1.0)
    re3 <- integral 110 0.8
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    --let mouseMove = mousePosition
    cam <- userCamera (Vec3 (-4) 0 0) mouseMove fblrPress
    return $ drawGLScene ssao vsm bloom fb rtex tex p scn <$> windowSize <*> re1 <*> re2 <*> re3 <*> cam <*> time <*> buttonPress

drawGLScene :: SSAO -> VSM -> Bloom -> GLFramebuffer -> GLColorTexture2D -> GLColorTexture2D -> GLProgram -> Scene -> (Int, Int)
            -> Float
            -> Float
            -> Float
            -> (Vec3, Vec3, Vec3, t1)
            -> Float
            -> Bool
            -> IO ()
drawGLScene ssao vsm bloom fb rtex tex p objs (w,h) _re1 re2 re3 (cam,dir,up,_) time buttonPress = do
    let cm = fromProjective (lookat cam (cam + dir) up)
        pm = perspective 0.1 50 90 (fromIntegral w / fromIntegral h)

        render t = withDepth $ do
            clearGLFramebuffer
            l <- forM objs $ \(worldMat,mesh) -> renderGLMesh' p mesh $ and <$> sequence
                -- setup uniforms
                [ setUniform p "cameraMatrix" $ U_Mat4 cm
                , setUniform p "projectionMatrix" $ U_Mat4 pm
                , setUniform p "time" $ U_Float time
                , setSampler p "diffuseTexture" t
                ]
            let b = and l
            unless b $ putStrLn "render fail"
            return b

    --withFramebuffer fb Nothing Nothing [rtex] $ render tex
    --render rtex
    --render tex

    --renderBloom bloom rtex

    --VSM
    renderVSM (w,h) time vsm objs cm pm
{ -
    --glViewport 0 0 (fromIntegral w) (fromIntegral h)
    -- SSAO
    if buttonPress then render tex else do
        withFramebuffer fb (Just $ ssaoDepthTexture ssao) Nothing [ssaoRenderTexture ssao] $ render tex
        renderSSAO ssao
- }
    swapBuffers

readInput :: State
          -> ((Float, Float) -> IO a)
          -> (Bool -> IO b)
          -> ((Bool, Bool, Bool, Bool, Bool) -> IO c)
          -> (Bool -> IO b)
          -> IO (Maybe Float)
readInput s mousePos mouseBut fblrPress buttonPress = do
    t <- getTime
    resetTime

    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    mouseBut =<< mouseButtonIsPressed MouseButton0
    fblrPress =<< ((,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift)
    buttonPress =<< keyIsPressed KeySpace

    updateFPS s t
    k <- keyIsPressed KeyEsc
    return $ if k then Nothing else Just (realToFrac t)
-}
keyboard :: ((Float, Float) -> IO a)
         -> (Bool -> IO b)
         -> ((Bool, Bool, Bool, Bool, Bool) -> IO c)
         -> (Bool -> IO b)
         -> KeyboardMouseCallback
keyboard mousePos mouseBut fblrPress buttonPress key keyState mods _  =   do
    (x,y) <- getMousePosition
    mousePos (fromIntegral x,fromIntegral y)

    mouseBut =<< mouseButtonIsPressed MouseButton0
    fblrPress =<< ((,,,,) <$> keyIsPressed KeyLeft <*> keyIsPressed KeyUp <*> keyIsPressed KeyDown <*> keyIsPressed KeyRight <*> keyIsPressed KeyRightShift)
    buttonPress =<< keyIsPressed KeySpace
    case (key, keyState) of
    {-
    (SpecialKey KeyLeft, Down) -> do
                       angle <- takeMVar $ viewAngle state
                       putMVar (viewAngle state) (angle+2)
    (SpecialKey KeyRight, Down) -> do
                       angle <- takeMVar $ viewAngle state
                       putMVar (viewAngle state) (angle-2)
    (SpecialKey KeyUp, Down) -> do
                       height <- takeMVar $ viewHeight state
                       putMVar (viewHeight state) (height-0.2)
    (SpecialKey KeyDown, Down) -> do
                       height <- takeMVar $ viewHeight state
                       putMVar (viewHeight state) (height+0.2)
    (Char 'p', Down) -> do
                       isPaused <- takeMVar $ paused state
                       putMVar (paused state) (not isPaused)
    -}
        (_, _) -> return ()
