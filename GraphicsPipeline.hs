{-# LANGUAGE OverloadedStrings #-}
module GraphicsPipeline where

import Data.Bitmap
import Data.ByteString.Char8 (ByteString)
import Data.Int
import Data.Vect
import Data.Vect.Float.Instances
import qualified Data.Vector.Storable as V

data AttributeType
    = AT_Float
    | AT_Vec2
    | AT_Vec3
    | AT_Vec4
    | AT_Mat2
    | AT_Mat3
    | AT_Mat4
    | AT_Int
    | AT_Word
    deriving (Eq,Ord,Show)

data Attribute
    = A_Float   (V.Vector Float)
    | A_Vec2    (V.Vector Vec2)
    | A_Vec3    (V.Vector Vec3)
    | A_Vec4    (V.Vector Vec4)
    | A_Mat2    (V.Vector Mat2)
    | A_Mat3    (V.Vector Mat3)
    | A_Mat4    (V.Vector Mat4)
    | A_Int     (V.Vector Int32)
    | A_Word    (V.Vector Word32)
    deriving (Eq,Show)

data UniformType
    = UT_Float
    | UT_Vec2
    | UT_Vec3
    | UT_Vec4
    | UT_Int
    | UT_Word
    | UT_Bool
    | UT_Mat2
    | UT_Mat3
    | UT_Mat4
    deriving (Eq,Ord,Show)

data SamplerType
    = ST_Sampler2D
    deriving (Eq,Ord,Show)

data Uniform
    = U_Float       Float
    | U_Vec2        Vec2
    | U_Vec3        Vec3
    | U_Vec4        Vec4
    | U_Int         Int32
    | U_Word        Word32
    | U_Bool        Bool
    | U_Mat2        Mat2
    | U_Mat3        Mat3
    | U_Mat4        Mat4

{-
required texture types:
    - depth (float)
    - rgb (float)
-}
data RenderBufferType
    = RBT_RGB8

data Program
    = Program
    { attributes        :: [(ByteString,AttributeType)]
    , uniforms          :: [(ByteString,UniformType)]
    , samplers          :: [(ByteString,SamplerType)]
    , outputs           :: [RenderBufferType]

    , vertexShader      :: [ByteString]
    , geometryShader    :: [ByteString]
    , fragmentShader    :: [ByteString]
    }

data Primitive
    = Points
    | TriangleStrip
    | Triangles
    | TriangleStripI (V.Vector Int)
    | TrianglesI (V.Vector Int)
    deriving (Eq,Show)

data Mesh
    = Mesh 
    { mAttributes :: [(ByteString,Attribute)]
    , mPrimitive  :: Primitive
    }
    deriving (Eq,Show)
