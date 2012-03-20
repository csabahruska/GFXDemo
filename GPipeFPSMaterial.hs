module GPipeFPSMaterial where

import Graphics.GPipe
import qualified Data.ByteString.Char8 as SB

identityLight :: Float
identityLight = 1

data Entity
    = Entity
    { eAmbientLight     :: Vec4 (Vertex Float)
    , eDirectedLight    :: Vec4 (Vertex Float)
    , eLightDir         :: Vec3 (Vertex Float)
    , eShaderRGBA       :: Vec4 (Vertex Float)
    }

data WaveType
    = WT_Sin
    | WT_Triangle
    | WT_Square
    | WT_Sawtooth
    | WT_InverseSawtooth
    | WT_Noise

data Wave = Wave WaveType Float Float Float Float

data Deform
    = D_AutoSprite
    | D_AutoSprite2
    | D_Bulge Float Float Float
    | D_Move (Vec3 Float) Wave
    | D_Normal Float Float
    | D_ProjectionShadow
    | D_Text0
    | D_Text1
    | D_Text2
    | D_Text3
    | D_Text4
    | D_Text5
    | D_Text6
    | D_Text7
    | D_Wave Float Wave

data CommonAttrs
    = CommonAttrs
    { caSkyParms        :: () -- TODO
    , caFogParms        :: () -- TODO
    , caPortal          :: Bool
    , caSort            :: Int -- default: 3 or 6 depends on blend function
    , caEntityMergable  :: Bool
    , caFogOnly         :: Bool
    , caCull            :: () -- TODO, default = front
    , caDeformVertexes  :: [Deform]
    , caNoMipMaps       :: Bool
    , caPolygonOffset   :: Maybe Float
    , caStages          :: [StageAttrs]
    }

defaultCommonAttrs :: CommonAttrs
defaultCommonAttrs = CommonAttrs
    { caSkyParms        = ()
    , caFogParms        = ()
    , caPortal          = False
    , caSort            = 3
    , caEntityMergable  = False
    , caFogOnly         = False
    , caCull            = ()
    , caDeformVertexes  = []
    , caNoMipMaps       = False
    , caPolygonOffset   = Nothing
    , caStages          = []
    }

data RGBGen
    = RGB_Wave Wave
    | RGB_Const Float Float Float
    | RGB_Identity
    | RGB_IdentityLighting
    | RGB_Entity
    | RGB_OneMinusEntity
    | RGB_ExactVertex
    | RGB_Vertex
    | RGB_LightingDiffuse
    | RGB_OneMinusVertex

data AlphaGen
    = A_Wave Wave
    | A_Const Float
    | A_Portal
    | A_Identity
    | A_Entity
    | A_OneMinusEntity
    | A_Vertex
    | A_LightingSpecular
    | A_OneMinusVertex

data TCGen
    = TG_Base
    | TG_Lightmap
    | TG_Environment -- TODO, check: RB_CalcEnvironmentTexCoords
    | TG_Vector (Vec3 Float) (Vec3 Float)

data TCMod
    = TM_EntityTranslate
    | TM_Rotate Float
    | TM_Scroll Float Float
    | TM_Scale Float Float
    | TM_Stretch Wave
    | TM_Transform Float Float Float Float Float Float
    | TM_Turb Float Float Float Float

data StageTexture
    = ST_Map        SB.ByteString
    | ST_ClampMap   SB.ByteString
    | ST_AnimMap    Float [SB.ByteString]
    | ST_Lightmap
    | ST_WhiteImage

data StageAttrs
    = StageAttrs
    { saBlend       :: Maybe (BlendingFactor,BlendingFactor)
    , saRGBGen      :: RGBGen
    , saAlphaGen    :: AlphaGen
    , saTCGen       :: TCGen
    , saTCMod       :: [TCMod]
    , saTexture     :: StageTexture
    , saDepthWrite  :: Bool
    , saDepthFunc   :: ComparisonFunction
    , saAlphaFunc   :: ()
    }

defaultStageAttrs :: StageAttrs
defaultStageAttrs = StageAttrs
    { saBlend       = Nothing
    , saRGBGen      = RGB_Identity
    , saAlphaGen    = A_Identity
    , saTCGen       = TG_Base
    , saTCMod       = []
    , saTexture     = ST_WhiteImage
    , saDepthWrite  = False
    , saDepthFunc   = Lequal
    , saAlphaFunc   = ()
    }

fixAttribOrder ca = ca
    { caDeformVertexes = reverse $ caDeformVertexes ca
    , caStages = reverse $ map fixStage $ caStages ca
    }
  where
    fixStage sa = sa
        { saTCMod = reverse $ saTCMod sa
        }

