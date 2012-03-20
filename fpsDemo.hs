{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Data.Attoparsec.Char8
import Data.IORef
import FRP.Elerea.Param
import GPipeFPSRender
import GPipeFPSMaterial
import GPipeUtils
import Graphics.GPipe
import Graphics.Rendering.OpenGL ( Position(..) )
import ShaderParser
import System.Directory
import System.FilePath
import Utils as U
import qualified BSPLoader as B
import qualified Data.ByteString.Char8 as SB
import qualified Data.Map as Map
import qualified Data.Trie as T
import qualified Data.Vec as Vec
import qualified Data.Vect.Float as V
import qualified Data.Vector as VC
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
                         keyboardMouseCallback,
                         elapsedTime,
                         get)


loadShaders :: IO (T.Trie CommonAttrs)
loadShaders = do
    l <- filter (\a -> ".shader" == takeExtension a) <$> getDirectoryContents "fps/scripts"
    sl <- forM l $ \n -> do
        s <- SB.readFile $ "fps/scripts/" ++ n
        return $ case parse shaders s of
            Done "" r -> r
            Fail _ c _   -> error $ show (n,"failed",c)
            Partial f -> case f "" of
                Done "" r -> r
                _   -> error $ show (n,"partial failed")
            Done rem r -> error $ show (n,"failed", map fst r)
    return $ T.fromList $ concat sl

parseEntities :: SB.ByteString -> [T.Trie SB.ByteString]
parseEntities s = case parse entities s of
    Done "" r -> r
    Fail _ c _   -> error $ show ("failed",c)
    Partial f -> case f "" of
        Done "" r -> r
        _   -> error $ show ("partial failed")
    Done rem r -> error $ show ("failed", r)

main :: IO ()
main = do
    getArgsAndInitialize

    -- setup FRP environment
    (winSize,winSizeSink) <- external (0,0)
    (mousePosition,mousePositionSink) <- external (0,0)
    (buttonPress,buttonPressSink) <- external False
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    --bsp <- B.loadBSP "fps/maps/cpm4a.bsp"
    --bsp <- B.loadBSP "fps/maps/ct3ctf2.bsp"
    --bsp <- B.loadBSP "fps/maps/q3ctf2.bsp"
    --bsp <- B.loadBSP "fps/maps/q3dm1.bsp"
    bsp <- B.loadBSP "fps/maps/q3dm7.bsp"
    --bsp <- B.loadBSP "fps/maps/q3dm3.bsp"
    --bsp <- B.loadBSP "fps/maps/q3dm11.bsp"
    --bsp <- B.loadBSP "fps/maps/q3dm4.bsp"
    --bsp <- B.loadBSP "fps/maps/q3dm18.bsp"
    --bsp <- B.loadBSP "fps/maps/q3tourney6.bsp"
    --bsp <- B.loadBSP "fps/maps/q3tourney3.bsp"
    --bsp <- B.loadBSP "fps/maps/SGDTT3.bsp"
    --bsp <- B.loadBSP "fps/maps/q3tourney1.bsp"
    --bsp <- B.loadBSP "fps/maps/erta1.bsp"

    -- extract spawn points
    let ents = parseEntities $ B.blEntities bsp
        spawn e = case T.lookup "classname" e of
            Just "info_player_deathmatch" -> True
            _ -> False
        Just sp0 = T.lookup "origin" $ head $ filter spawn ents
        [x0,y0,z0] = map read $ words $ SB.unpack sp0
        p0 = V.Vec3 x0 z0 (-y0)

    shaders <- loadShaders
    let gr b = compileBSP shaders b
        g = gr bsp
    print $ VC.length g
    net <- start $ scene p0 bsp g mousePosition fblrPress buttonPress winSize
    keys <- newIORef $ Map.empty
    tr <- newIORef =<< get elapsedTime

    putStrLn "creating window..."
    newWindow "FPS Demo" 
        (100:.100:.()) 
        (800:.600:.()) 
        (renderFrame tr keys fblrPressSink buttonPressSink winSizeSink net)
        (initWindow keys mousePositionSink)
    putStrLn "entering mainloop..."
    mainLoop

--renderFrame :: Vec2 Int -> IO (FrameBuffer RGBFormat () ())
renderFrame tr keys fblrPress buttonPress winSize net (w:.h:.()) = do
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
    t <- get elapsedTime
    t0 <- readIORef tr
    let d = (fromIntegral $ t-t0) / 1000
    writeIORef tr t
    join $ net d 

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
scene p0 bsp surfaces mousePosition fblrPress buttonPress wh = do
    time <- stateful 0 (+)
    last2 <- transfer ((0,0),(0,0)) (\_ n (_,b) -> (b,n)) mousePosition
    let mouseMove = (\((ox,oy),(nx,ny)) -> (nx-ox,ny-oy)) <$> last2
    --let mouseMove = mousePosition
    cam <- userCamera p0 mouseMove fblrPress
    return $ drawGLScene bsp surfaces <$> wh <*> cam <*> time <*> buttonPress

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
drawGLScene bsp surfaces (w,h) (cam,dir,up,_) time buttonPress = do
    let scal = 0.01
        near = 0.1
        far  = 500
        fovDeg = 90
        fovRad = fovDeg * pi / 180
        cm = (V.fromProjective (lookat cam (cam + dir) up)) V..*. (V.fromProjective $ V.scaling (V.Vec3 scal scal scal))
        pm = U.perspective near far fovRad (fromIntegral w / fromIntegral h)
        culledSurfaces = cullSurfaces bsp cam frust surfaces
        frust = frustum fovDeg (fromIntegral w / fromIntegral h) (near) (far) cam (cam+dir) up
    print ("cull nums",VC.length surfaces,VC.length culledSurfaces)
    return $ renderSurfaces time (toGPU time) (convMat (cm V..*. pm)) culledSurfaces

-- Key -> KeyState -> Modifiers -> Position -> IO ()
keyboard keys mousePos key keyState mods (Position x y) = do
    mousePos (fromIntegral x,fromIntegral y)
    modifyIORef keys $ \m -> case (key, keyState) of
        (c, Down) -> Map.insert c True m
        (c, Up) -> Map.insert c False m
