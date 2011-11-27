{-# LANGUAGE OverloadedStrings #-}

module ShaderParser where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import Data.List (foldl')
import GPipeFPS
import GPipeUtils
import Graphics.GPipe hiding ((<*))
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T

-- utility parsers
shaderName :: Parser ByteString
shaderName = word

skip :: Parser ()
skip = skipSpace <* many (comment <* skipSpace)

skipRest = skipWhile (\c -> c /= '\n' && c /= '\r')

comment = (stringCI "//" <* skipWhile (\c -> c /= '\n' && c /= '\r')) <|> (string "/*" <* manyTill anyChar (try (string "*/")))

word :: Parser ByteString
word = skip *> takeTill isSpace

kw :: ByteString -> Parser ()
kw s = ((\w -> if B.map toLower w == s then return () else fail "") =<< word) <?> B.unpack s

val :: a -> ByteString -> Parser a
val v w = const v <$> kw w

float :: Parser Float
float = (\_ c a -> c * read a) <$> skip <*> option 1 ((const 1 <$> char '+') <|> (const (-1) <$> char '-')) <*>
    ( ((\a _ b -> a ++ b) <$> many1 digit <*> char '.' <*> many1 digit) <|>
      ((\_ a -> "0." ++ a) <$> char '.' <*> many1 digit) <|>
      (many1 digit) )
    
int :: Parser Int
int = skip *> decimal

-- q3 entity description parser
entities :: Parser [T.Trie ByteString]
entities = skipSpace *> many entity <* skipSpace

entity :: Parser (T.Trie ByteString)
entity = T.fromList <$> (kw "{" *> many ((,) <$> str <*> str) <* kw "}")

str :: Parser ByteString
str = skipSpace *> string "\"" *> takeWhile1 (\c -> c /= '"') <* char '"'

-- q3 shader related parsers

waveFun = val sinTexture "sin" <|>
          val triangleTexture "triangle" <|>
          val squareTexture "square" <|>
          val sawToothTexture "sawtooth" <|>
          val inverseSawToothTexture "inversesawtooth" <|>
          val (error "noise wave function is not supported") "noise"

shaders :: Parser [(ByteString,(Int,Renderer))]
shaders = skip *> many shader <* skip

shader :: Parser (ByteString,(Int,Renderer))
shader = (\n _ l _ -> (n,stagesRenderer $ foldl' (\s f -> f s) defaultCommonAttrs l)) <$> word <*> kw "{" <*> many shaderAttrs <*> kw "}"

shaderAttrs :: Parser (CommonAttrs -> CommonAttrs)
shaderAttrs = general <|> q3map <|> editor <|> stage

{-
general =
    skyParms
    fogParms
    portal
    sort
    entityMergable
    fogonly
    cull
    deformVertexes
    nopicmip
    nomipmaps
    polygonOffset
-}

{-
stageAttrs =
    mapP        - texture source
    clampMap    - texture source
    animMap     - texture source

    blendFunc   - paint function parameter
    rgbGen
    alphaGen
    alphaFunc
    tcGen       - vertex function
    tcMod       - vertex function
    depthFunc   - paint function parameter
    depthWrite  - paint function parameter
    detail
-}

pass _ a = a

general = cull      <|> deformVertexes <|> entityMergable <|> fogParms <|> fogonly  <|>
          nomipmaps <|> nopicmip       <|> polygonOffset  <|> portal   <|> skyParms <|> sort

q3map = q3MapSun <|> surfaceParm <|> light <|> lightning <|> cloudparams <|> sky <|> foggen <|>
        tessSize <|> (pass <$> (skip *> stringCI "q3map_" *> skipWhile (\c -> c /= '\n' && c /= '\r')))

editor = pass <$> (skip *> stringCI "qer_" *> skipWhile (\c -> c /= '\n' && c /= '\r'))

stage = (\_ fl _ ca -> addRenderer ca $ foldl' (\s f -> f s) defaultStageAttrs fl) <$> kw "{" <*> many stageAttrs <*> kw "}"

stageAttrs :: Parser (StageAttrs -> StageAttrs)
stageAttrs = alphaFunc  <|> alphaGen <|> animMap <|> blendFunc <|> clampMap  <|> depthFunc <|> 
             depthWrite <|> detail   <|> mapP    <|> rgbGen    <|> tcGen     <|> tcMod

--
-- General Shader Keywords
--

fogonly = pass <$> kw "fogonly"

{-
skyParms <farbox> <cloudheight> <nearbox>
  <farbox>:
    "-"         - no farbox
    “env/test”  - would look for files “env/test_rt.tga”, “env/test_lf.tga”, “env/test_ft.tga”, “env/test_bk.tga”, “env/test_up.tga”, “env/test_dn.tga”

  <nearbox>:
    “-“ - ignore (This has not been tested in a long time)
-}

skyParms = pass <$> kw "skyparms" <* (kw "-" <|> (const () <$> word)) <* (kw "-" <|> (const () <$> word)) <* kw "-"

cull = pass <$> kw "cull" <* (kw "front" <|> kw "back" <|> kw "disable" <|> kw "none" <|> kw "twosided" <|> kw "backsided")

deformVertexes = kw "deformvertexes" *> (
    pass <$> kw "autosprite" <|>
    pass <$> kw "autosprite2" <|>
    pass <$> kw "bulge" <* float <* float <* float <|>
    pass <$> kw "move" <* float <* float <* float <* waveFun <* float <* float <* float <* float <|>
    pass <$> kw "normal" <* float <* float <|> -- amplitude, frequency
    pass <$> kw "projectionshadow" <|>
    pass <$> kw "text0" <|>
    pass <$> kw "text1" <|>
    pass <$> kw "text2" <|>
    pass <$> kw "text3" <|>
    pass <$> kw "text4" <|>
    pass <$> kw "text5" <|>
    pass <$> kw "text6" <|> 
    pass <$> kw "text7" <|>
    pass <$> kw "wave" <* float <* waveFun <* float <* float <* float <* float
    )

fogParms = pass <$> kw "fogparms" <* option () (kw "(") <* float <* float <* float <* option () (kw ")") <* float <* skipRest
nopicmip = pass <$> kw "nopicmip"
nomipmaps = (\_ ca -> ca {caNoMipMaps = True}) <$> kw "nomipmaps"
entityMergable = pass <$> kw "entitymergable"
polygonOffset = pass <$> kw "polygonoffset"
portal = pass <$> kw "portal"

-- sort portal|sky|opaque|banner|underwater|additive|nearest|[number]
sort = (\_ i ca -> ca {caSort = i}) <$> kw "sort" <*> (
    val 1 "portal"     <|>
    val 2 "sky"        <|>
    val 3 "opaque"     <|>
    val 6 "banner"     <|>
    val 8 "underwater" <|>
    val 9 "additive"   <|>
    val 16 "nearest"   <|>
    int)

--
-- Stage Specific Keywords
--

{-
one stage is one pass
question: can we render in single pass?
answer: yes, but the backend should optimize it. Now we should build multipass rendering.
-}

mapP = (\_ v sa -> sa {saTexture = v}) <$> kw "map" <*> (
    val ST_Lightmap "$lightmap" <|> 
    val ST_WhiteImage "$whiteimage" <|> 
    ST_Map <$> word
    )

clampMap = (\v sa -> sa {saTexture = ST_ClampMap v}) <$> (kw "clampmap" *> word)

animMap = (\_ f v sa -> sa {saTexture = ST_AnimMap f v}) <$> kw "animmap" <*> float <*> (B.words <$> takeTill fun)--many1 (skipWhile fun *> takeTill fun) -- FIXME: comment is not supported!
  where
    fun c = c == '\n' || c == '\r'

blendFuncFunc = val (One,One) "add"
            <|> val (DstColor,Zero) "filter"
            <|> val (SrcAlpha,OneMinusSrcAlpha) "blend"

srcBlend = val One "gl_one"
       <|> val Zero "gl_zero"
       <|> val DstColor "gl_dst_color"
       <|> val OneMinusDstColor "gl_one_minus_dst_color"
       <|> val SrcAlpha "gl_src_alpha"
       <|> val OneMinusSrcAlpha "gl_one_minus_src_alpha"
       <|> val DstAlpha "gl_dst_alpha"
       <|> val OneMinusDstAlpha "gl_one_minus_dst_alpha"
       <|> val SrcAlphaSaturate "gl_src_alpha_saturate"

dstBlend = val One "gl_one"
       <|> val Zero "gl_zero"
       <|> val SrcAlpha "gl_src_alpha"
       <|> val OneMinusSrcAlpha "gl_one_minus_src_alpha"
       <|> val DstAlpha "gl_dst_alpha"
       <|> val OneMinusDstAlpha "gl_one_minus_dst_alpha"
       <|> val SrcColor "gl_src_color"
       <|> val OneMinusSrcColor "gl_one_minus_src_color"

blendFunc = (\_ b sa -> sa {saBlend = Just b}) <$> kw "blendfunc" <*> choice [blendFuncFunc, (,) <$> srcBlend <*> dstBlend]

{-
identity - RGBA 1 1 1 1
identity_lighting (identity_light_byte = ilb) - RGBA ilb ilb ilb ilb
lighting_diffuse - ??? check: RB_CalcDiffuseColor
exact_vertex - vertex color
const - constant color
vertex (identity_light = il, vertex_color*il) - RGBA (r*il) (g*il) (b*il) a
one_minus_vertex = (identity_light = il, vertex_color*il) - RGBA ((1-r)*il) ((1-g)*il) ((1-b)*il) a
fog - fog color
waveform (c = clamp 0 1 (wave_value * identity_light)) - RGBA c c c 1
entity - entity's shaderRGB
one_minus_entity - 1 - entity's shaderRGB
-}

rgbExactVertex _ (_,_,_,r:.g:.b:._:.()) = r:.g:.b:.()
rgbIdentity _ _ = toGPU (1:.1:.1:.())
rgbIdentityLighting _ _ = toGPU (identityLight:.identityLight:.identityLight:.())
rgbConst r g b _ _ = toGPU (r:.g:.b:.())
rgbVertex _ (_,_,_,r:.g:.b:._:.()) = f r:.f g:.f b:.()
  where
    f a = toGPU identityLight * a
rgbOneMinusVertex _ (_,_,_,r:.g:.b:._:.()) = f r:.f g:.f b:.()
  where
    f a = 1 - toGPU identityLight * a

rgbGen = (\_ v sa -> sa {saRGBGen = v}) <$> kw "rgbgen" <*> (
    val rgbIdentity "wave" <* waveFun <* float <* float <* float <* float <|> -- TODO
    (\_ _ r g b _ -> rgbConst r g b) <$> kw "const" <*> kw "(" <*> float <*> float <*> float <*> kw ")" <|>
    val rgbIdentity "identity" <|> 
    val rgbIdentityLighting "identitylighting" <|> 
    val rgbIdentity "entity" <|>  -- TODO
    val rgbIdentity "oneminusentity" <|> -- TODO
    val rgbExactVertex "exactvertex" <|> 
    val rgbVertex "vertex" <|> 
    val rgbIdentity "lightingdiffuse" <|> -- TODO
    val rgbOneMinusVertex "oneminusvertex"
    )

{-
identity - alpha = 1
const - constant alpha
wave - clamped waveform
lightingspecular - ??? check: RB_CalcSpecularAlpha
entity - entity's shaderRGBA's alpha
oneminusentity - 1 - entity's shaderRGBA's alpha
vertex - vertex alpha
oneminusvertex - 1 - vertex alpha
portal - ???
-}
alphaIdentity _ _ = 1
alphaConst a _ _ = toGPU a
alphaVertex _ (_,_,_,_:._:._:.a:.()) = a
alphaOneMinusVertex _ (_,_,_,_:._:._:.a:.()) = 1 - a

alphaGen = (\_ v sa -> sa {saAlphaGen = v}) <$> kw "alphagen" <*> (
    val alphaIdentity "wave" <* waveFun <* float <* float <* float <* float <|> -- TODO
    (\_ a -> alphaConst a) <$> kw "const" <*> float <|>
    val alphaIdentity "portal" <* float <|> -- TODO
    val alphaIdentity "identity" <|> 
    val alphaIdentity "entity" <|> -- TODO
    val alphaIdentity "oneminusentity" <|> -- TODO
    val alphaVertex "vertex" <|>
    val alphaIdentity "lightingspecular" <|> -- TODO
    val alphaOneMinusVertex "oneminusvertex"
    )

tcGen = (\_ v sa -> sa {saTCGen = v}) <$> kw "tcgen" <*> (
    val (\(_,uv,_,_) -> uv) "base"
    <|> val (\(_,_,uv,_) -> uv) "lightmap"
    <|> val (\(_,uv,_,_) -> uv) "environment" -- TODO, check: RB_CalcEnvironmentTexCoords
    <|> ((\_ u v -> (\(p,_,_,_) -> (dot p u):.(dot p v):.())) <$> kw "vector" <*> v3 <*> v3))
  where
    v3 = (\_ x y z _ -> toGPU $ x:.y:.z:.()) <$> kw "(" <*> float <*> float <*> float <*> kw ")"

tcMod = (\_ v sa -> sa {saTCMod = v:saTCMod sa}) <$> kw "tcmod" <*> (
    val idfun "entitytranslate" <|>
    val idfun "rotate" <* float <* skipRest <|>
    (\_ su sv t (u:.v:.()) -> fract' (u+t*toGPU su):.fract' (v+t*toGPU sv):.()) <$> kw "scroll" <*> float <*> float <* skipRest <|>
    (\_ su sv _ (u:.v:.()) -> (u*toGPU su):.(v*toGPU sv):.()) <$> kw "scale" <*> float <*> float <* skipRest <|>
    val idfun "stretch" <* waveFun <* float <* float <* float <* float <|>
    val idfun "transform" <* float <* float <* float <* float <* float <* float <|>
    val idfun "turb" <* option () (kw "sin") <* float <* float <* float <* float
    )
  where
    idfun = \_ uv -> uv

depthFunc = (\_ v sa -> sa {saDepthFunc = v}) <$> kw "depthfunc" <*> (val Lequal "lequal" <|> val Equal "equal")
depthWrite = (\_ sa -> sa {saDepthWrite = True}) <$> kw "depthwrite"
detail = pass <$> kw "detail"
alphaFunc = pass <$> kw "alphafunc" <* (kw "gt0" <|> kw "lt128" <|> kw "ge128")

--
-- Q3MAP Specific Shader Keywords
--
cloudparams = pass <$> kw "cloudparms" <* skipRest
lightning = pass <$> kw "lightning" <* skipRest
light = pass <$> (kw "light" <|> kw "light1") <* skipRest
sky = pass <$> kw "sky" <* skipRest
foggen = pass <$> kw "foggen" <* skipRest

tessSize = pass <$> kw "tesssize" <* float

-- q3map_sun <red> <green> <blue> <intensity> <degrees> <elevation>
q3MapSun = pass <$> kw "q3map_sun" <* float <* float <* float <* float <* float <* float

surfaceParm = pass <$> kw "surfaceparm" <* (
        kw "water"      <|> kw "slime"      <|> kw "lava"           <|> kw "playerclip" <|> kw "monsterclip"
    <|> kw "nodrop"     <|> kw "nonsolid"   <|> kw "origin"         <|> kw "trans"      <|> kw "detail"
    <|> kw "structural" <|> kw "areaportal" <|> kw "clusterportal"  <|> kw "donotenter" <|> kw "fog"
    <|> kw "sky"        <|> kw "lightfilter"<|> kw "alphashadow"    <|> kw "hint"       <|> kw "botclip"
    <|> kw "slick"      <|> kw "noimpact"   <|> kw "nomarks"        <|> kw "ladder"     <|> kw "nodamage"
    <|> kw "metalsteps" <|> kw "flesh"      <|> kw "nosteps"        <|> kw "nodraw"     <|> kw "antiportal"
    <|> kw "pointlight" <|> kw "nolightmap" <|> kw "nodlight"       <|> kw "dust"       <|> kw "lightgrid"
    <|> kw "nopicmip"   <|> kw "nomipmaps")
