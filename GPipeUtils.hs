{-# LANGUAGE OverloadedStrings #-}
module GPipeUtils where

import Graphics.GPipe
import qualified Data.ByteString.Char8 as SB
import qualified Data.Vector.Storable as V

import Binary.Fast
import GraphicsPipeline
import qualified Data.Vect as Vect

loadGPipeMesh :: String -> IO (PrimitiveStream Triangle (Vec3 (Vertex Float)))
loadGPipeMesh n = do
    Mesh al Triangles <- loadMesh $ SB.pack n
    let A_Vec3 av = head [a | ("position",a) <- al]
    return $ toGPUStream TriangleList [x:.y:.z:.() | Vect.Vec3 x y z <- V.toList av]
