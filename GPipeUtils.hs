{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module GPipeUtils where

import Control.Monad
import Foreign
import Data.IORef
import Codec.Image.STB
import Data.Bitmap.Pure (bitmapToByteString)
import Data.Bitmap.IO
import Foreign.Ptr
import Graphics.GPipe
import System.Directory
import System.FilePath.Posix
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Trie.Convenience as T
import qualified Data.Vector.Storable as V

import Binary.Fast
import GraphicsPipeline
import Utils (cube')
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

textureFromByteString :: Bool -> Int -> Int -> Int -> SB.ByteString -> Texture2D RGBAFormat
textureFromByteString mipmap c w h s = unsafePerformIO $ do
    bm <- case c of
        3   -> SB.useAsCString (addAlpha s) $ \p -> copyBitmapFromPtr (w,h) 4 0 (castPtr p) Nothing :: IO (Bitmap Word8)
        4   -> SB.useAsCString s $ \p -> copyBitmapFromPtr (w,h) 4 0 (castPtr p) Nothing
        _   -> error "unsupported texture format!"
    let sizes x y = if nx == 0 || ny == 0 then [] else (nx,ny):sizes nx ny
          where nx = x `div` 2
                ny = y `div` 2
    mips <- forM (if mipmap then sizes w h else []) $ \wh -> do
        b <- bilinearResample bm wh Nothing
        return $ bitmapToByteString b
    let mips' = [] -- FIXME: MipMap handling is not working in GPipe
    withMany SB.useAsCString (bitmapToByteString bm:mips') $ \pl -> newTexture (PerComp4 UnsignedByteFormat) RGBA8 (w:.h:.()) $ map castPtr pl
{-
textureFromByteString mipmap 3 w h s = unsafePerformIO $ SB.useAsCString (addAlpha s) $ \p -> newTexture (PerComp4 UnsignedByteFormat) RGBA8 (w:.h:.()) [castPtr p]
textureFromByteString mipmap 4 w h s = unsafePerformIO $ SB.useAsCString s $ \p -> newTexture (PerComp4 UnsignedByteFormat) RGBA8 (w:.h:.()) [castPtr p]
textureFromByteString _ _ _ _ _ = error "unsupported texture format!"
-}

textureCache :: IORef (T.Trie (Texture2D RGBAFormat))
{-# NOINLINE textureCache #-}
textureCache = unsafePerformIO (newIORef T.empty)

textureFromFile :: Bool -> String -> Texture2D RGBAFormat
textureFromFile mipmap name = unsafePerformIO $ do
    let namesb = SB.pack name
    ic <- readIORef textureCache
    case T.lookup namesb ic of
        Just i  -> do
            --putStrLn ("Cached texture: " ++ name)
            return i
        Nothing -> do
            i <- loadImage name
            case i of
                Left s -> putStrLn ("Could not load: " ++ name ++ " due to: " ++ s) >> return defaultImage
                Right img -> do
                    putStrLn ("Load texture: " ++ name)
                    let (w,h) = bitmapSize img
                        tx = textureFromByteString mipmap (bitmapNChannels img) w h (bitmapToByteString img)
                    writeIORef textureCache $ T.insertIfAbsent namesb tx ic
                    return $ seq tx tx

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

whiteImage = textureFromByteString False 4 8 8 $ SB.replicate (8*8*4) '\255'
defaultImage = textureFromByteString True 4 16 16 $ SB.pack $ concatMap (replicate 4) [if e x || e y then '\255' else '\32' | y <- [0..15], x <- [0..15]]
  where
    e  0 = True
    e 15 = True
    e  _ = False

loadQ3Texture :: Bool -> String -> Texture2D RGBAFormat
loadQ3Texture mipmap name' = unsafePerformIO $ do
    let name = "fps/" ++ name'
        n1 = replaceExtension name "tga"
        n2 = replaceExtension name "jpg"
    b0 <- doesFileExist name
    b1 <- doesFileExist n1
    b2 <- doesFileExist n2
    return $ textureFromFile mipmap $ if b0 then name else if b1 then n1 else n2

pointToCube c0 (p,t1,t2,c) = [(p+cubep,t1,t2,c0) | cubep <- pl]
  where
    Mesh al (TrianglesI idx) = cube'
    A_Vec3 av = head [a | (an,a) <- al, an == SB.pack "position"]
    pl = [(2*x):.(2*y):.(2*z):.() | Vect.Vec3 x y z <- V.toList $ V.backpermute (V.convert av) (V.convert idx)]
