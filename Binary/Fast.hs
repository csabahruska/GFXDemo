{-# OPTIONS_GHC -fno-warn-orphans #-}
module Binary.Fast where

import Control.Applicative
import Data.Binary
import Data.ByteString.Char8 (ByteString)
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector.Storable as V

import GraphicsPipeline
import ThriftUtils

instance Storable a => Binary (V.Vector a) where
    put v = put $ vToSB v
    get = do s <- get ; return $ sblToV [s]

fileVersion :: Int32
fileVersion = 1

loadMesh :: ByteString -> IO Mesh
loadMesh n = decode <$> LB.readFile (SB.unpack n)

saveMesh :: ByteString -> Mesh -> IO ()
saveMesh n m = LB.writeFile (SB.unpack n) (encode m)

instance Binary Attribute where
    put (A_Float a) = putWord8 0 >> put a
    put (A_Vec2 a)  = putWord8 1 >> put a
    put (A_Vec3 a)  = putWord8 2 >> put a
    put (A_Vec4 a)  = putWord8 3 >> put a
    put (A_Mat2 a)  = putWord8 4 >> put a
    put (A_Mat3 a)  = putWord8 5 >> put a
    put (A_Mat4 a)  = putWord8 6 >> put a
    put (A_Int a)   = putWord8 7 >> put a
    put (A_Word a)  = putWord8 8 >> put a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> A_Float <$> get
            1 -> A_Vec2  <$> get
            2 -> A_Vec3  <$> get
            3 -> A_Vec4  <$> get
            4 -> A_Mat2  <$> get
            5 -> A_Mat3  <$> get
            6 -> A_Mat4  <$> get
            7 -> A_Int   <$> get
            8 -> A_Word  <$> get
            _ -> fail "no parse"

putIndices :: V.Vector Int -> Put
putIndices i = put $ V.map (fromIntegral :: Int -> Int32) i

getIndices :: Get (V.Vector Int)
getIndices = V.map (fromIntegral :: Int32 -> Int) <$> get

instance Binary Primitive where
    put Points             = putWord8 0
    put TriangleStrip      = putWord8 1
    put Triangles          = putWord8 2
    put (TriangleStripI a) = putWord8 3 >> putIndices a
    put (TrianglesI a)     = putWord8 4 >> putIndices a
    get = do
        tag_ <- getWord8
        case tag_ of
            0 -> return Points
            1 -> return TriangleStrip
            2 -> return Triangles
            3 -> TriangleStripI <$> getIndices
            4 -> TrianglesI <$> getIndices
            _ -> fail "no parse"

instance Binary Mesh where
    put (Mesh a b) = put a >> put b
    get = Mesh <$> get <*> get
