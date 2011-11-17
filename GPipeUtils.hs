{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module GPipeUtils where

import Graphics.GPipe
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector.Storable as V
import Foreign.Ptr
import System.IO.Unsafe
import Codec.Image.STB
import Data.Bitmap.Pure
import System.Directory
import System.FilePath.Posix

import Binary.Fast
import GraphicsPipeline
import qualified Data.Vect as Vect

loadGPipeMesh :: String -> IO (PrimitiveStream Triangle (Vec3 (Vertex Float)))
loadGPipeMesh n = do
    Mesh al Triangles <- loadMesh $ SB.pack n
    let A_Vec3 av = head [a | ("position",a) <- al]
    return $ toGPUStream TriangleList [x:.y:.z:.() | Vect.Vec3 x y z <- V.toList av]

addAlpha :: SB.ByteString -> SB.ByteString
addAlpha s = SB.pack $ go $ SB.unpack s
  where
    a = '\255'
    go (r:g:b:xs) = r:g:b:a:go xs
    go _ = []

textureFromByteString :: Int -> Int -> Int -> SB.ByteString -> Texture2D RGBAFormat
textureFromByteString 3 w h s = unsafePerformIO $ SB.useAsCString (addAlpha s) $ \p -> newTexture (PerComp4 UnsignedByteFormat) RGBA8 (w:.h:.()) [castPtr p]
textureFromByteString 4 w h s = unsafePerformIO $ SB.useAsCString s $ \p -> newTexture (PerComp4 UnsignedByteFormat) RGBA8 (w:.h:.()) [castPtr p]
textureFromByteString _ _ _ _ = error "unsupported texture format!"

textureFromFile :: String -> Texture2D RGBAFormat
textureFromFile name = unsafePerformIO $ do
    i <- loadImage name
    case i of
        Left s -> putStrLn ("Could not load: " ++ name ++ " due to: " ++ s) >> return defaultImage
        Right img -> do
            putStrLn ("Load texture: " ++ name)
            let (w,h) = bitmapSize img
            return $ textureFromByteString (bitmapNChannels img) w h (bitmapToByteString img)

tableTexture :: [Float] -> Texture1D LuminanceFormat
tableTexture t = unsafePerformIO $ V.unsafeWith (V.fromList t) $ \p -> newTexture FloatFormat Luminance16 (length t) [castPtr p]

funcTableSize = 1024 :: Float
sinTexture = tableTexture [sin (i*2*pi/(funcTableSize-1)) | i <- [0..funcTableSize-1]]
squareTexture = tableTexture [if i < funcTableSize / 2 then 1 else -1 | i <- [0..funcTableSize-1]]
sawToothTexture = tableTexture [i / funcTableSize | i <- [0..funcTableSize-1]]
inverseSawToothTexture = tableTexture $ reverse [i / funcTableSize | i <- [0..funcTableSize-1]]
triangleTexture = tableTexture $ l1 ++ map ((-1)*) l1
  where
    n = funcTableSize / 4
    l0 = [i / n | i <- [0..n-1]]
    l1 = l0 ++ reverse l0

whiteImage = textureFromByteString 4 8 8 $ SB.replicate (8*8*4) '\255'
defaultImage = textureFromByteString 4 16 16 $ SB.pack $ concatMap (replicate 4) [if e x || e y then '\255' else '\32' | y <- [0..15], x <- [0..15]]
  where
    e  0 = True
    e 15 = True
    e  _ = False

loadQ3Texture name' = unsafePerformIO $ do
    let name = "fps/" ++ name'
        n1 = replaceExtension name "tga"
        n2 = replaceExtension name "jpg"
    b0 <- doesFileExist name
    b1 <- doesFileExist n1
    b2 <- doesFileExist n2
    return $ textureFromFile $ if b0 then name else if b1 then n1 else n2
