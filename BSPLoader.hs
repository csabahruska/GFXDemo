{-# LANGUAGE OverloadedStrings #-}
module BSPLoader where

import Data.Word
import Data.Int
import Data.Bits
import Control.Monad
import Control.Applicative

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.ByteString as SB8
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect hiding (Vector)

--import Utils

{-
Information: http://graphics.stanford.edu/~kekoa/q3/

Data types

Quake 3 BSP files contains only four basic data types. They are:

Type        Description
ubyte       unsigned byte
int         4-byte integer, little-endian
float       4-byte IEEE float, little-endian
string[n]   string of n ASCII bytes, not necessarily null-terminated

All data in a BSP file is organized into records composed of these four data types.
-}

-- http://www.mralligator.com/q3/

bspEntities         = 0 :: Int  -- ^ Game-related object descriptions
bspTextures         = 1 :: Int  -- ^ Stores texture information
bspPlanes           = 2 :: Int  -- ^ Stores the splitting planes
bspNodes            = 3 :: Int  -- ^ Stores the BSP nodes
bspLeaves           = 4 :: Int  -- ^ Stores the leafs of the nodes
bspLeafFaces        = 5 :: Int  -- ^ Stores the leaf's indices into the faces
bspLeafBrushes      = 6 :: Int  -- ^ Stores the leaf's indices into the brushes
bspModels           = 7 :: Int  -- ^ Descriptions of rigid world geometry in map
bspBrushes          = 8 :: Int  -- ^ Stores the brushes info (for collision)
bspBrushSides       = 9 :: Int  -- ^ Stores the brush surfaces
bspVertices         = 10 :: Int -- ^ Stores the level vertices
bspMeshIndices      = 11 :: Int -- ^ Stores the level indices
bspEffects          = 12 :: Int -- ^ List of special map effects
bspFaces            = 13 :: Int -- ^ Stores the faces for the level
bspLightmaps        = 14 :: Int -- ^ Stores the lightmaps for the level
bspLightVols        = 15 :: Int -- ^ Local illumination data
bspVisData          = 16 :: Int -- ^ Stores PVS and cluster info (visibility)

data Texture
    = Texture
    { txName     :: SB.ByteString
    , txFlags    :: Int
    , txContents :: Int
    }

data Plane
    = Plane
    { plNormal  :: Vec3
    , plDist    :: Float
    }

data Node
    = Node
    { ndPlane    :: Int
    , ndChildren :: (Int,Int)
    , ndMins     :: Vec3
    , ndMaxs     :: Vec3
    }

data Leaf
    = Leaf
    { lfCluster         :: Int
    , lfArea            :: Int
    , lfMins            :: Vec3
    , lfMaxs            :: Vec3
    , lfLeafFace        :: Int
    , lfNumLeafFaces    :: Int
    , lfLeafBrush       :: Int
    , lfNumLeafBrushes  :: Int
    }

data Model
    = Model
    { mdMins        :: Vec3
    , mdMaxs        :: Vec3
    , mdFace        :: Int
    , mdNumFaces    :: Int
    , mdBrush       :: Int
    , mdNumBrushes  :: Int
    }

data Brush
    = Brush
    { brBrushSide       :: Int
    , brNumBrushSides   :: Int
    , brTexture         :: Int
    }

data BrushSide
    = BrushSide
    { bsPlane   :: Int
    , bsTexture :: Int
    }

data Vertex
    = Vertex
    { vrPosition    :: Vec3
    , vrTexCoord0   :: Vec2
    , vrTexCoord1   :: Vec2
    , vrNormal      :: Vec3
    , vrColor       :: Vec4
    }

data Effect
    = Effect
    { efName    :: SB.ByteString
    , efBrush   :: Int
    , efUnknown :: Int
    }

data Face
    = Face
    { fcTexture         :: Int
    , fcEffect          :: Int
    , fcType            :: Int
    , fcVertex          :: Int
    , fcNumVertices     :: Int
    , fcMeshVert        :: Int
    , fcNumMeshVerts    :: Int
    , fcLMIndex         :: Int
    , fcLMStrart        :: Vec2
    , fcLMSize          :: Vec2
    , fcLMOrigin        :: Vec3
    , fcLMVecU          :: Vec3
    , fcLMVecV          :: Vec3
    , fcNormal          :: Vec3
    , fcSize            :: Vec2
    }

data LightMap
    = LightMap
    { lmMap :: SB.ByteString
    }

data LightVol
    = LightVol

data VisData
    = VisData
    { vdNumVecs     :: Int
    , vdSizeVecs    :: Int
    , vdVecs        :: Vector Word8
    }

data BSPLevel
    = BSPLevel
    { blEntities    :: SB.ByteString
    , blTextures    :: Vector Texture
    , blPlanes      :: Vector Plane
    , blNodes       :: Vector Node
    , blLeafs       :: Vector Leaf
    , blLeafFaces   :: Vector Int
    , blLeafBrushes :: Vector Int
    , blModels      :: Vector Model
    , blBrushes     :: Vector Brush
    , blBrushSides  :: Vector BrushSide
    , blVertices    :: Vector Vertex
    , blMeshVerts   :: Vector Int
    , blEffects     :: Vector Effect
    , blFaces       :: Vector Face
    , blLightMaps   :: Vector LightMap
    , blLightVols   :: Vector LightVol
    , blVisData     :: VisData
    }

getString   = fmap (SB.takeWhile (/= '\0')) . getByteString

getWord     = getWord32le

getUByte    = B.get :: Get Word8
getUByte2   = B.get :: Get (Word8,Word8)
getUByte3   = B.get :: Get (Word8,Word8,Word8)

getFloat    = getFloat32le

getVec2     = Vec2 <$> getFloat <*> getFloat
getVec3     = (\x y z -> Vec3 x z (-y)) <$> getFloat <*> getFloat <*> getFloat
getVec2i    = (\x y -> Vec2 (fromIntegral x) (fromIntegral y)) <$> getInt <*> getInt
getVec3i    = (\x y z -> Vec3 (fromIntegral x) (fromIntegral z) (fromIntegral (-y))) <$> getInt <*> getInt <*> getInt

getVec4RGBA = (\r g b a -> Vec4 (f r) (f g) (f b) (f a)) <$> getUByte <*> getUByte <*> getUByte <*> getUByte
  where
    f v = fromIntegral v / 255

getInt      = fromIntegral <$> getInt' :: Get Int
  where
    getInt' = fromIntegral <$> getWord32le :: Get Int32

getInt2     = (,) <$> getInt <*> getInt

getItems s act l = V.fromList <$> replicateM (l `div` s) act

getHeader = do
    magic <- getString 4
    case magic == "IBSP" of
        True    -> return ()
        _       -> error "Invalid format."
    version <- getWord
    el <- replicateM 17 getInt2
    return (magic,version,el)

getBSPLevel el = BSPLevel
    <$> getLump getEntities      bspEntities
    <*> getLump getTextures      bspTextures
    <*> getLump getPlanes        bspPlanes
    <*> getLump getNodes         bspNodes
    <*> getLump getLeaves        bspLeaves
    <*> getLump getLeafFaces     bspLeafFaces
    <*> getLump getLeafBrushes   bspLeafBrushes
    <*> getLump getModels        bspModels
    <*> getLump getBrushes       bspBrushes
    <*> getLump getBrushSides    bspBrushSides
    <*> getLump getVertices      bspVertices
    <*> getLump getMeshIndices   bspMeshIndices
    <*> getLump getEffects       bspEffects
    <*> getLump getFaces         bspFaces
    <*> getLump getLightmaps     bspLightmaps
    <*> getLump getLightVols     bspLightVols
    <*> getLump getVisData       bspVisData
  where
    getLump g i = lookAhead $ do
        let (o,l) = el !! i
        skip o
        g l

getEntities l   = getString l
getTextures     = getItems  72 $ Texture    <$> getString 64 <*> getInt <*> getInt
getPlanes       = getItems  16 $ Plane      <$> getVec3 <*> getFloat
getNodes        = getItems  36 $ Node       <$> getInt <*> getInt2 <*> getVec3i <*> getVec3i
getLeaves       = getItems  48 $ Leaf       <$> getInt <*> getInt <*> getVec3i <*> getVec3i <*> getInt <*> getInt <*> getInt <*> getInt
getLeafFaces    = getItems   4   getInt
getLeafBrushes  = getItems   4   getInt
getModels       = getItems  40 $ Model      <$> getVec3 <*> getVec3 <*> getInt <*> getInt <*> getInt <*> getInt
getBrushes      = getItems  12 $ Brush      <$> getInt <*> getInt <*> getInt
getBrushSides   = getItems   8 $ BrushSide  <$> getInt <*> getInt
getVertices     = getItems  44 $ Vertex     <$> getVec3 <*> getVec2 <*> getVec2 <*> getVec3 <*> getVec4RGBA
getMeshIndices  = getItems   4   getInt
getEffects      = getItems  72 $ Effect     <$> getString 64 <*> getInt <*> getInt
getFaces        = getItems 104 $ Face       <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt
                                            <*> getVec2i <*> getVec2i <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec3 <*> getVec2i
getLightmaps    = getItems (128*128*3) (LightMap <$> (getByteString $ 128*128*3))

getLightVols = getItems 8 $ do
    ambient     <- getUByte3
    directional <- getUByte3
    dir         <- getUByte2
    return LightVol

getVisData l = do
    nvecs   <- getInt
    szvecs  <- getInt
    vecs    <- getByteString $ nvecs * szvecs
    return $ VisData nvecs szvecs $ V.fromList $ SB8.unpack vecs

loadBSP :: String -> IO BSPLevel
loadBSP n = readBSP <$> LB.readFile n

readBSP dat = do
    let (magic,version,el) = runGet getHeader dat
    runGet (getBSPLevel el) dat
