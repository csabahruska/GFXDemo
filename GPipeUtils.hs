{-# LANGUAGE OverloadedStrings #-}
module GPipeUtils where

import Control.Monad
import Foreign
import Data.IORef
import Codec.Image.STB
import Data.Bitmap.Pure (bitmapToByteString)
import Data.Bitmap.IO
import Foreign.Ptr
import Graphics.GPipe
import System.IO.Unsafe
import qualified Data.ByteString.Char8 as SB
import qualified Data.Trie as T
import qualified Data.Trie.Convenience as T
import qualified Data.Vector.Storable as V

import Binary.Fast
import GraphicsPipeline
import Utils (cube')
import qualified Data.Vect as Vect
import qualified Data.Vec as Vec

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

textureCache :: IORef (T.Trie (Texture2D RGBAFormat))
{-# NOINLINE textureCache #-}
textureCache = unsafePerformIO (newIORef T.empty)

textureFromFile :: Bool -> String -> Maybe (Texture2D RGBAFormat)
textureFromFile mipmap name = unsafePerformIO $ do
    let namesb = SB.pack name
    ic <- readIORef textureCache
    case T.lookup namesb ic of
        Just i  -> do
            --putStrLn ("Cached texture: " ++ name)
            return $ Just i
        Nothing -> do
            i <- loadImage name
            case i of
                Left s -> putStrLn ("Could not load: " ++ name ++ " due to: " ++ s) >> return Nothing
                Right img -> do
                    putStrLn ("Load texture: " ++ name)
                    let (w,h) = bitmapSize img
                        tx = textureFromByteString mipmap (bitmapNChannels img) w h (bitmapToByteString img)
                    writeIORef textureCache $ T.insertIfAbsent namesb tx ic
                    return $ Just $ seq tx tx

pointToCube c0 (p,t1,t2,c) = [(p+cubep,t1,t2,c0) | cubep <- pl]
  where
    Mesh al (TrianglesI idx) = cube'
    A_Vec3 av = head [a | (an,a) <- al, an == SB.pack "position"]
    pl = [(2*x):.(2*y):.(2*z):.() | Vect.Vec3 x y z <- V.toList $ V.backpermute (V.convert av) (V.convert idx)]

data Frustum
    = Frustum
    { frPlanes :: Vec.Vec6 (Vect.Vec3, Float)
    , ntl :: Vect.Vec3
    , ntr :: Vect.Vec3
    , nbl :: Vect.Vec3
    , nbr :: Vect.Vec3
    , ftl :: Vect.Vec3
    , ftr :: Vect.Vec3
    , fbl :: Vect.Vec3
    , fbr :: Vect.Vec3
    }

pointInFrustum p fr = Vec.foldr (\(n,d) b -> b && d + n `Vect.dotprod` p >= 0) True $ frPlanes fr

sphereInFrustum p r fr = Vec.foldr (\(n,d) b -> b && d + n `Vect.dotprod` p >= (-r)) True $ frPlanes fr

boxInFrustum pp pn fr = Vec.foldr (\(n,d) b -> b && d + n `Vect.dotprod` (g pp pn n) >= 0) True $ frPlanes fr
  where
    g (Vect.Vec3 px py pz) (Vect.Vec3 nx ny nz) n = Vect.Vec3 (fx px nx) (fy py ny) (fz pz nz)
      where
        Vect.Vec3 x y z = n
        fx:.fy:.fz:.() = Vec.map (\a -> if a > 0 then max else min) (x:.y:.z:.())

frustum :: Float -> Float -> Float -> Float -> Vect.Vec3 -> Vect.Vec3 -> Vect.Vec3 -> Frustum
frustum angle ratio nearD farD p l u = Frustum ((pl ntr ntl ftl):.(pl nbl nbr fbr):.(pl ntl nbl fbl):.
                                                (pl nbr ntr fbr):.(pl ntl ntr nbr):.(pl ftr ftl fbl):.()) ntl ntr nbl nbr ftl ftr fbl fbr
  where
    pl a b c = (n,d)
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
