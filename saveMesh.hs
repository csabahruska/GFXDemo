{-# LANGUAGE OverloadedStrings #-}
import Binary.Fast
import Control.Monad
import Data.ByteString.Char8 (ByteString)
import System.Environment
import ThriftUtils

main :: IO ()
main = do
    l <- getArgs
    forM_ (map SB.pack l) $ \n -> do
        m <- remoteMesh n
        saveMesh (SB.append n ".lcmesh") m
--        m' <- loadMesh (SB.append n ".lcmesh")
--        print $ m == m'
