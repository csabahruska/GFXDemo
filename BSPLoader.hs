{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module BSPLoader where

import Data.Word
import Data.Int
import Data.Bits
import Control.Monad
import Control.Applicative

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.ByteString.Char8 as SB
import qualified Data.ByteString.Lazy as LB
import Data.Binary as B
import Data.Binary.Get as B
import Data.Binary.IEEE754
import Data.Vect

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
    { txName     :: ByteString
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
    { efName    :: ByteString
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
    { lmMap :: ByteString
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
    { blEntities    :: ByteString
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

getString   = fmap (S.unpack . S.takeWhile (/= '\0')) . getByteString

getWord     = getWord32le

getUByte    = B.get :: Get Word8
getUByte2   = B.get :: Get (Word8,Word8)
getUByte3   = B.get :: Get (Word8,Word8,Word8)

getFloat    = getFloat32le

getVec3 f   = (\x y z -> x:.(z):.(-y):.()) <$> f <*> f <*> f

getVec2f    = (\x y -> x:.y:.()) <$> getFloat <*> getFloat
getVec3f    = getVec3 getFloat

getInt'     = fromIntegral <$> getWord32le :: Get Int32
getInt      = fromIntegral <$> getInt' :: Get Int
getInt2     = (,) <$> getInt <*> getInt
getVec2i    = (\x y -> x:.y:.()) <$> getInt <*> getInt
getVec3i    = getVec3 getInt

getVec4b    = (\x y z w -> x:.y:.z:.w:.()) <$> getUByte <*> getUByte <*> getUByte <*> getUByte

getItems s act l = V.fromList <$> replicateM (l `div` s) act
getItemsUV s act l = UV.fromList <$> replicateM (l `div` s) act

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
getTextures     = getItems 72  $ Texture <$> getString 64 <*> getInt <*> getInt
getPlanes       = getItems 16  $ Plane <$> getVec3f <*> getFloat
getNodes        = getItems 36  $ Node <$> getInt <*> getVec2i <*> getVec3i <*> getVec3i
getLeaves       = getItems 48  $ Leaf <$> getInt <*> getInt <*> (Vec.map fromIntegral <$> getVec3i)
                                      <*> (Vec.map fromIntegral <$> getVec3i) <*> getInt <*> getInt <*> getInt <*> getInt
getLeafFaces    = getItemsUV 4   getInt
getLeafBrushes  = getItems 4     getInt
getModels       = getItems 40  $ Model <$> getVec3f <*> getVec3f <*> getInt <*> getInt <*> getInt <*> getInt
getBrushes      = getItems 12  $ Brush <$> getInt <*> getInt <*> getInt
getBrushSides   = getItems 8   $ BrushSide <$> getInt <*> getInt
getVertices     = getItems 44  $ Vertex <$> getVec3f <*> getVec2f <*> getVec2f <*> getVec3f <*> getVec4b
getMeshIndices  = getItems 4     getInt
getEffects      = getItems 72  $ Effect <$> getString 64 <*> getInt <*> getInt
getFaces        = getItems 104 $ Face <$> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt <*> getInt
                                      <*> getVec2i <*> getVec2i <*> getVec3f <*> getVec3f <*> getVec3f <*> getVec3f <*> getVec2i
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
    return $ VisData nvecs szvecs $ UV.fromList $ BS.unpack vecs

loadBSP :: String -> IO BSPLevel
loadBSP n = readBSP <$> L.readFile n

readBSP dat = do
    let (magic,version,el) = runGet getHeader dat
    runGet (getBSPLevel el) dat
