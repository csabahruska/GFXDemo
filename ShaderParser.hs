{-# LANGUAGE OverloadedStrings #-}

module ShaderParser where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Combinator
import Data.ByteString.Char8 (ByteString)
import Data.Char (toLower)
import qualified Data.ByteString.Char8 as B

-- utility parsers
shaderName :: Parser ByteString
shaderName = word

skip :: Parser ()
skip = skipSpace <* many (comment <* skipSpace)

comment = (stringCI "//" <* skipWhile (\c -> c /= '\n' && c /= '\r')) <|> (string "/*" <* manyTill anyChar (try (string "*/")))

word :: Parser ByteString
word = skip *> takeTill isSpace

kw :: ByteString -> Parser ()
kw s = ((\w -> if B.map toLower w == s then return () else fail "") =<< word) <?> B.unpack s

float :: Parser Float
float = (\_ c a -> c * read a) <$> skip <*> option 1 ((const 1 <$> char '+') <|> (const (-1) <$> char '-')) <*>
    ( ((\a _ b -> a ++ b) <$> many1 digit <*> char '.' <*> many1 digit) <|>
      ((\_ a -> "0." ++ a) <$> char '.' <*> many1 digit) <|>
      (many1 digit) )
    
int :: Parser Int
int = skip *> decimal

-- q3 shader related parsers

waveFun = kw "sin" <|> kw "triangle" <|> kw "square" <|> kw "sawtooth" <|> kw "inversesawtooth" <|> kw "noise"

shaders = many shader <* skipSpace

shader = word <*
    kw "{" <*
        many (general <|> q3map <|> editor <|> stage) <*
    kw "}"

general = skyParms  <|> cull        <|> deformVertexes  <|> fogParms    <|> 
          nopicmip  <|> nomipmaps   <|> polygonOffset   <|> portal      <|> sort <|>
          entityMergable

q3map = q3MapSun <|> surfaceParm <|> 
        tessSize <|> (skip *> stringCI "q3map_" *> skipWhile (\c -> c /= '\n' && c /= '\r'))

editor = (skip *> stringCI "qer_" *> skipWhile (\c -> c /= '\n' && c /= '\r'))

stage = kw "{" <* many stageAttrs <* kw "}"
stageAttrs  = mapP  <|> clampMap    <|> animMap     <|> blendFunc   <|> rgbGen  <|> alphaGen    <|>
              tcGen <|> tcMod       <|> depthFunc   <|> depthWrite  <|> detail  <|> alphaFunc

--
-- General Shader Keywords
--

{-
skyParms <farbox> <cloudheight> <nearbox>
  <farbox>:
    "-"         - no farbox
    “env/test”  - would look for files “env/test_rt.tga”, “env/test_lf.tga”, “env/test_ft.tga”, “env/test_bk.tga”, “env/test_up.tga”, “env/test_dn.tga”

  <nearbox>:
    “-“ - ignore (This has not been tested in a long time)
-}

skyParms = kw "skyparms" <* (kw "-" <|> (const () <$> word)) <* (float <|> (const 0 <$> kw "-")) <* kw "-"

cull = kw "cull" <* (kw "front" <|> kw "back" <|> kw "disable" <|> kw "none" <|> kw "twosided")

deformVertexes = kw "deformvertexes" <* (
    kw "wave" <* float <* waveFun <* float <* float <* float <* float <|>
    kw "normal" <* float <* float <|>
    kw "bulge" <* float <* float <* float <|>
    kw "move" <* float <* float <* float <* waveFun <* float <* float <* float <* float <|>
    kw "autosprite" <|>
    kw "autosprite2" <|>
    kw "projectionshadow"
    )

fogParms = kw "fogparms" <* kw "(" <* float <* float <* float <* kw ")" <* float

nopicmip = kw "nopicmip"

nomipmaps = kw "nomipmaps"

entityMergable = kw "entitymergable"

polygonOffset = kw "polygonoffset"

portal = kw "portal"

-- sort portal|sky|opaque|banner|underwater|additive|nearest|[number]
sort = kw "sort" <* (kw "portal" <|> kw "sky" <|> kw "opaque" <|> kw "banner" <|> kw "underwater" <|> kw "additive" <|> kw "nearest" <|> (const () <$> int))

--
-- Q3MAP Specific Shader Keywords
--

tessSize = kw "tesssize" <* float

-- q3map_sun <red> <green> <blue> <intensity> <degrees> <elevation>
q3MapSun = kw "q3map_sun" <* float <* float <* float <* float <* float <* float

surfaceParm = kw "surfaceparm" <* (
        kw "water"      <|> kw "slime"      <|> kw "lava"           <|> kw "playerclip" <|> kw "monsterclip"
    <|> kw "nodrop"     <|> kw "nonsolid"   <|> kw "origin"         <|> kw "trans"      <|> kw "detail"
    <|> kw "structural" <|> kw "areaportal" <|> kw "clusterportal"  <|> kw "donotenter" <|> kw "fog"
    <|> kw "sky"        <|> kw "lightfilter"<|> kw "alphashadow"    <|> kw "hint"       <|> kw "botclip"
    <|> kw "slick"      <|> kw "noimpact"   <|> kw "nomarks"        <|> kw "ladder"     <|> kw "nodamage"
    <|> kw "metalsteps" <|> kw "flesh"      <|> kw "nosteps"        <|> kw "nodraw"     <|> kw "antiportal"
    <|> kw "pointlight" <|> kw "nolightmap" <|> kw "nodlight"       <|> kw "dust"       <|> kw "lightgrid"
    )

--
-- Stage Specific Keywords
--

mapP = kw "map" <* (kw "$lightmap" <|> kw "$whiteimage" <|> (const () <$> word))

clampMap = kw "clampmap" <* word

animMap = kw "animmap" <* (B.words <$> takeTill fun)--many1 (skipWhile fun *> takeTill fun) -- FIXME: comment is not supported!
  where
    fun c = c == '\n' || c == '\r'

blendFuncFunc = choice [kw "add", kw "filter", kw "blend"]
srcBlend      = choice [kw "gl_one", kw "gl_zero", kw "gl_dst_color", kw "gl_one_minus_dst_color", kw "gl_src_alpha", kw "gl_one_minus_src_alpha",
                        kw "gl_dst_alpha", kw "gl_one_minus_dst_alpha", kw "gl_src_alpha_saturate"]
dstBlend      = choice [kw "gl_one", kw "gl_zero", kw "gl_src_alpha", kw "gl_one_minus_src_alpha", kw "gl_dst_alpha", kw "gl_one_minus_dst_alpha", 
                        kw "gl_src_color", kw "gl_one_minus_src_color"]
blendFunc     = kw "blendfunc" <* choice [blendFuncFunc, srcBlend <* dstBlend]

rgbGen = kw "rgbgen" <* (
    kw "wave" <* waveFun <* float <* float <* float <* float
    <|> kw "const" <* kw "(" <* float <* float <* float <* kw ")"
    <|> kw "identity"   <|> kw "identitylighting"   <|> kw "entity"             <|> kw "oneminusentity"
    <|> kw "exactvertex" <|> kw "vertex"            <|> kw "lightingdiffuse"    <|> kw "oneminusvertex"
    )

alphaGen = kw "alphagen" <* (
    kw "wave" <* waveFun <* float <* float <* float <* float
    <|> kw "const" <* float
    <|> kw "portal" <* float
    <|> kw "identity"       <|> kw "identitylighting"   <|> kw "entity"             <|> kw "oneminusentity"
    <|> kw "fromvertex"     <|> kw "vertex"             <|> kw "lightingdiffuse"    <|> kw "lightingspecular"
    <|> kw "oneminusvertex"
    )

tcGen = kw "tcgen" <* (
    kw "base" <|> kw "lightmap" <|> kw "environment" <|>
    (kw "vector" <* kw "(" <* float <* float <* float <* kw ")" <* kw "(" <* float <* float <* float <* kw ")")
    )

tcMod = kw "tcmod" <* (
    kw "rotate" <* float <|>
    kw "scale" <* float <* float <|>
    kw "scroll" <* float <* float <|>
    kw "stretch" <* waveFun <* float <* float <* float <* float <|>
    kw "transform" <* float <* float <* float <* float <* float <* float <|>
    kw "turb" <* float <* float <* float <* float <|>
    kw "entitytranslate"
    )

depthFunc = kw "depthfunc" <* (kw "lequal" <|> kw "equal")

depthWrite = kw "depthwrite"

detail = kw "detail"

alphaFunc = kw "alphafunc" <* (kw "gt0" <|> kw "lt128" <|> kw "ge128")
