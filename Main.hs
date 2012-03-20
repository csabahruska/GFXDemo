{-# LANGUAGE OverloadedStrings, PackageImports #-}

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Binary.Fast
import Codec.Image.STB
import Control.Applicative
import Control.Monad
import Data.Bitmap.Pure
import Data.ByteString.Char8 (ByteString)
import Data.Vect
import FRP.Elerea.Param
import GLBackend
import GraphicsPipeline
import ThriftUtils
import Utils
import qualified Data.ByteString.Char8 as SB
import Graphics.Rendering.OpenGL.Raw.Core32

setupProgram :: IO GLProgram
setupProgram = do
    vsrc <- SB.readFile "simple/simple.vert"
    fsrc <- SB.readFile "simple/simple.frag"
    Just p <- compileProgram $ Program 
        { attributes        = [("position",AT_Vec3)
                              ,("normal",AT_Vec3)
                              ,("UVTex",AT_Vec2)]
        , uniforms          = [("cameraMatrix",UT_Mat4)
                              ,("projectionMatrix",UT_Mat4)
                              ,("time",UT_Float)]
        , samplers          = [("diffuseTexture",ST_Sampler2D)]
        , outputs           = []
        , vertexShader      = [vsrc]
        , geometryShader    = []
        , fragmentShader    = [fsrc]
        }
    return p

type Scene = [(Mat4,GLMesh)]

main :: IO ()
main = do
    windowSize <- initCommon "Graphics Pipeline Demo"

    (mousePosition,mousePositionSink) <- external (0,0)
    (_mousePress,mousePressSink) <- external False
    (buttonPress,buttonPressSink) <- external False
    (fblrPress,fblrPressSink) <- external (False,False,False,False,False)

    printGLVersion
    glProgram <- setupProgram
    --glCube <- compileMesh cube
    --glCube <- compileMesh =<< remoteMesh "Cube"
    --glCube <- compileMesh =<< remoteMesh "Monkey"
    glScene <- compileMesh =<< loadMesh "Scene.lcmesh"
    glCube <- compileMesh =<< loadMesh "Monkey.lcmesh"
    glPlane <- compileMesh =<< loadMesh "Plane.lcmesh"
    glSphere <- compileMesh =<< loadMesh "Icosphere.lcmesh"
    let scn = [(idmtx,glSphere),(idmtx,glCube),(idmtx,glPlane),(idmtx,glScene)]
    Right b <- loadImage "textures/Panels_Diffuse.png"
    diffuseTex <- compileColorTexture2D b
    renderTex <- compileColorTexture2D $ emptyBitmap (512,512) 3 Nothing
    fb <- newGLFramebuffer
    bloom <- initBloom
    vsm <- initVSM
    ssao <- initSSAO
    s <- fpsState
    sc <- start $ scene ssao vsm bloom fb renderTex diffuseTex glProgram scn windowSize mousePosition fblrPress buttonPress
    driveNetwork sc (readInput s mousePositionSink mousePressSink fblrPressSink buttonPressSink)

    closeWindow

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
{-
    --glViewport 0 0 (fromIntegral w) (fromIntegral h)
    -- SSAO
    if buttonPress then render tex else do
        withFramebuffer fb (Just $ ssaoDepthTexture ssao) Nothing [ssaoRenderTexture ssao] $ render tex
        renderSSAO ssao
-}
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

program :: Program
program = Program [] [] [] [] [] [] []

loadProgram :: FilePath -> FilePath -> Program -> IO GLProgram
loadProgram v f p = do
    vsrc <- SB.readFile v
    fsrc <- SB.readFile f
    SB.putStr "Loading shaders: "
    print (v,f)
    Just glp <- compileProgram $ p {vertexShader = [vsrc], fragmentShader = [fsrc]}
    return glp

-- Variance Shadow Map Effect
data VSM
    = VSM
    { vsmDepthTexture   :: GLColorTexture2D
    , vsmBlurTexture    :: GLColorTexture2D
    , vsmFramebuffer    :: GLFramebuffer
    , vsmStoreProgram   :: GLProgram
    , vsmBlurProgram    :: GLProgram
    , vsmShadowProgram  :: GLProgram
    , vsmShadowMapSize  :: Size
    , vsmTmpDepth       :: GLDepthTexture2D
    }

initVSM :: IO VSM
initVSM = do
    storeProgram <- loadProgram "vsm/StoreDepth.vert" "vsm/StoreDepth.frag" $ program
        { attributes = [("position",        AT_Vec3)]
        , uniforms   = [("worldProjection", UT_Mat4)]
        }
    blurProgram <- loadProgram "vsm/Blur.vert" "vsm/Blur.frag" $ program
        { attributes = [("position",        AT_Vec3)
                       ,("uv",              AT_Vec2)]
        , uniforms   = [("worldProjection", UT_Mat4)
                       ,("scaleU",          UT_Vec2)]
        , samplers   = [("textureSource",   ST_Sampler2D)]
        }
    shadowProgram <- loadProgram "vsm/Final.vert" "vsm/Final.frag" $ program
        { attributes = [("position",        AT_Vec3)]
        , uniforms   = [("worldProjection", UT_Mat4)
                       ,("lightProjection", UT_Mat4)]
        , samplers   = [("shadowMap",       ST_Sampler2D)]
        }
    let smSize     = (512,512)
        --blurW      = shadowMapH * coeff
        --blurH      = shadowMapW * coeff
        --coeff      = 0.25
        blurTexture = undefined
    framebuffer  <- newGLFramebuffer
    depthTexture <- compileColorTexture2D $ emptyBitmap smSize 3 Nothing
    --blurTexture  <- compileColorTexture2D $ emptyBitmap (floor blurW,floor blurH) 3 Nothing
    tmpDepth <- newGLDepthTexture2D smSize
    return $ VSM depthTexture blurTexture framebuffer storeProgram blurProgram shadowProgram smSize tmpDepth

renderVSM :: Size -> Float -> VSM -> Scene -> Mat4 -> Mat4 -> IO Bool
renderVSM (w,h) time vsm ((_,o):(_,m):ox) camMat projMat = do
    let fb   = vsmFramebuffer vsm
        dtex = vsmDepthTexture vsm
        btex = vsmBlurTexture vsm
        stp  = vsmStoreProgram vsm
        bp   = vsmBlurProgram vsm
        shp  = vsmShadowProgram vsm
        lpos = Vec3 0.1 2 0.1
        lat  = (Vec3 0 (-100) 0)
        up   = Vec3 0 1 0
        objs = (fromProjective $ (rotationEuler (Vec3 (time*0.1) (time*0.3) 0) .*. translation lpos),o):
               (fromProjective $ rotationEuler (Vec3 time (time/pi) 0),m):ox -- (fromProjective $ translation lat,o):ox
        lmat = fromProjective (lookat lpos lat up)
        scale a b s = a + (b-a)*(s/2+0.5)
        frontclip = scale (0.00001) 0.8 $ sin (time*0.6)
        frontclip' = 0.4
        pmat = perspective frontclip' 5 90 (fromIntegral w / fromIntegral h)
        (sw,sh) = vsmShadowMapSize vsm
        {-
        bias = Mat4 (Vec4 0.5 0.0 0.0 0.0)
                    (Vec4 0.0 0.5 0.0 0.0)
                    (Vec4 0.0 0.0 0.5 0.0)
                    (Vec4 0.5 0.5 0.5 1.0)
        -}
    --print frontclip
    -- render from light view, store depth map
    oks1 <- withFramebuffer fb (Just $ vsmTmpDepth vsm) Nothing [dtex] $ withDepth $ do
        glViewport 0 0 (fromIntegral sw) (fromIntegral sh)
        clearGLFramebuffer
        l <- forM objs $ \(worldMat,mesh) ->
            renderGLMesh' stp mesh $ setUniform stp "worldProjection" $ U_Mat4 $ worldMat .*. lmat .*. pmat
--            renderGLMesh' stp mesh $ setUniform stp "worldProjection" $ U_Mat4 $ worldMat .*. camMat .*. projMat
        return $ and l

    -- blur depth map
    --  blurH
    --  blurV
    {-
    oks2 <- withFramebuffer fb Nothing Nothing [btex] $ do
        clearGLFramebuffer
        l <- forM objs $ \(worldMat,mesh) ->
            renderGLMesh' strp mesh $ setUniform strp "worldProjection" $ U_Mat4 worldMat -- TODO: set proper matrix
        return $ and l
    -}

    -- render from camera view
    oks3 <- withDepth $ do
        glViewport 0 0 (fromIntegral w) (fromIntegral h)
        clearGLFramebuffer
        l <- forM objs $ \(worldMat,mesh) -> renderGLMesh' shp mesh $ and <$> sequence
                -- setup uniforms
                --[ setUniform shp "worldProjection" $ U_Mat4 $ worldMat .*. lmat .*. pmat
                [ setUniform shp "worldProjection" $ U_Mat4 $ worldMat .*. camMat .*. projMat
                , setUniform shp "lightProjection" $ U_Mat4 $ worldMat .*. lmat .*. pmat
                --, setUniform shp "lightProjection" $ U_Mat4 $ worldMat .*. camMat .*. projMat
                , setSampler shp "shadowMap" dtex
                ]
        return $ and l
    return False

-- Bloom Effect
data Bloom
    = Bloom
    { glmProgram :: GLProgram
    , glmQuad    :: GLMesh
    }

initBloom :: IO Bloom
initBloom = do
    bloomProgram <- loadProgram "bloom/bloom.vert" "bloom/bloom.frag" $ program
        { attributes = [("position",        AT_Vec2)]
        , samplers   = [("renderedTexture", ST_Sampler2D)]
        }
    quad <- compileMesh quad2D
    return $ Bloom bloomProgram quad

renderBloom :: Bloom -> GLColorTexture2D -> IO Bool
renderBloom (Bloom p m) tex = do
    clearGLFramebuffer
    b <- renderGLMesh' p m $ setSampler p "renderedTexture" tex
    unless b $ putStrLn "render fail"
    return b

{-
    { vsmDepthTexture   :: GLColorTexture2D
    , vsmBlurTexture    :: GLColorTexture2D
    , vsmFramebuffer    :: GLFramebuffer
    , vsmStoreProgram   :: GLProgram
    , vsmBlurProgram    :: GLProgram
    , vsmShadowProgram  :: GLProgram
    , vsmShadowMapSize  :: Size
    , vsmTmpDepth       :: GLDepthTexture2D
    }
-}
-- SSAO Effect
data SSAO
    = SSAO
    { ssaoProgram          :: GLProgram
    , ssaoQuad             :: GLMesh
    , ssaoDepthTexture     :: GLDepthTexture2D
    , ssaoLuminanceTexture :: GLColorTexture2D
    , ssaoRenderTexture    :: GLColorTexture2D
    , ssaoRenderSize       :: Size
    }

initSSAO :: IO SSAO
initSSAO = do
    ssaoProgram <- loadProgram "ssao/ssao.vert" "ssao/ssao.frag" $ program
        { attributes = [("position",                  AT_Vec2)]
        , uniforms   = [("bgl_RenderedTextureHeight", UT_Float)
                       ,("bgl_RenderedTextureWidth",  UT_Float)]
        , samplers   = [("bgl_DepthTexture",          ST_Sampler2D)
                       ,("bgl_LuminanceTexture",      ST_Sampler2D)
                       ,("bgl_RenderedTexture",       ST_Sampler2D)]
        }
    quad <- compileMesh quad2D
    let size = (1280,720)
    depthTexture <- newGLDepthTexture2D size
    luminanceTexture <- compileColorTexture2D $ emptyBitmap size 3 Nothing
    renderTexture <- compileColorTexture2D $ emptyBitmap size 3 Nothing
    return $ SSAO ssaoProgram quad depthTexture luminanceTexture renderTexture size

--renderSSAO :: SSAO -> Size -> GLDepthTexture2D -> GLColorTexture2D -> GLColorTexture2D -> IO Bool
--renderSSAO ssao (w,h) dt rt lt = do
renderSSAO ssao = do
    clearGLFramebuffer
    let p = ssaoProgram ssao
        lt = ssaoLuminanceTexture ssao
        (w,h) = ssaoRenderSize ssao
        rt = ssaoRenderTexture ssao
        dt = ssaoDepthTexture ssao
    b <- renderGLMesh' p (ssaoQuad ssao) $ and <$> sequence
        -- setup uniforms
        [ setUniform p "bgl_RenderedTextureHeight" $ U_Float $ fromIntegral h
        , setUniform p "bgl_RenderedTextureWidth" $ U_Float $ fromIntegral w
        , setSampler p "bgl_DepthTexture" dt
        , setSampler p "bgl_LuminanceTexture" lt
        , setSampler p "bgl_RenderedTexture" rt
        ]
    unless b $ putStrLn "render fail"
    return b
