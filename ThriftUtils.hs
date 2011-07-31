{-# LANGUAGE TupleSections #-}
module ThriftUtils (remoteMesh,sblToV,vToSB) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.ByteString.Char8 (ByteString)
import Foreign
import Network
import Network.URI
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Internal as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import qualified GraphicsPipeline as GFX

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport.Handle
import Thrift.Transport.HttpClient

import Thrift.ContentProvider_Client
import Thrift.Content_Types

import System.IO.Unsafe

sblToV :: Storable a => [SB.ByteString] -> V.Vector a
sblToV ls = v
  where
    offs o (s:xs) = (o,s):offs (o + SB.length s) xs
    offs _ [] = []
    cnt = sum (map SB.length ls) `div` (sizeOf $ V.head v)
    v = unsafePerformIO $ do
        mv <- MV.new cnt
        MV.unsafeWith mv $ \dst -> forM_ (offs 0 ls) $ \(o,s) ->
            SB.useAsCStringLen s $ \(src,len) -> moveBytes (plusPtr dst o) src len
        V.unsafeFreeze mv

vToSB :: Storable a => V.Vector a -> SB.ByteString
vToSB v = unsafePerformIO $ do
    let len = V.length v * sizeOf (V.head v)
    V.unsafeWith v $ \p -> SB.packCStringLen (castPtr p,len)

toV :: Storable a => [LB.ByteString] -> V.Vector a
toV lb = sblToV $ concatMap LB.toChunks lb

unpackAttribute :: VertexAttribute -> (ByteString,GFX.Attribute)
unpackAttribute (VertexAttribute (Just an) (Just at) (Just ad)) = (SB.pack an,) $ case at of
    AT_Float -> GFX.A_Float $ toV ad
    AT_Vec2  -> GFX.A_Vec2  $ toV ad
    AT_Vec3  -> GFX.A_Vec3  $ toV ad
    AT_Vec4  -> GFX.A_Vec4  $ toV ad
    AT_Mat2  -> GFX.A_Mat2  $ toV ad
    AT_Mat3  -> GFX.A_Mat3  $ toV ad
    AT_Mat4  -> GFX.A_Mat4  $ toV ad
    AT_Int   -> GFX.A_Int   $ toV ad
    AT_Word  -> GFX.A_Word  $ toV ad 

remoteMesh :: ByteString -> IO GFX.Mesh
remoteMesh name = do
    let toVInt :: V.Vector Int32 -> V.Vector Int
        toVInt = V.map fromIntegral
    p <- BinaryProtocol <$> hOpen ("localhost", PortNumber 9090)
    Mesh (Just attrs) (Just prim) idx <- downloadMesh (p,p) $ SB.unpack name
    return $ GFX.Mesh (map unpackAttribute attrs) $ case (prim,idx) of
            (PT_Points,Nothing)        -> GFX.Points
            (PT_TriangleStrip,Nothing) -> GFX.TriangleStrip
            (PT_Triangles,Nothing)     -> GFX.Triangles
            (PT_TriangleStrip,Just i)  -> GFX.TriangleStripI $ toVInt $ toV i
            (PT_Triangles,Just i)      -> GFX.TrianglesI $ toVInt $ toV i
            _                          -> error "Invalid primitive!"
