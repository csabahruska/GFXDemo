{-# LANGUAGE OverloadedStrings, TupleSections #-}
module GLBackend where

import Control.Applicative
import Control.Monad
import Data.Bitmap
import Data.Bitmap.IO
import Data.ByteString.Char8 (ByteString)
import Data.List
import Data.Maybe
import Data.Trie (Trie)
--import Data.Vect
--import Data.Vect.Float.Instances
import Foreign
import qualified Data.ByteString.Char8 as SB
import qualified Data.Set as Set
import qualified Data.Trie as T
import qualified Data.Vector.Storable as V

import Graphics.Rendering.OpenGL.Raw.Core32
import GraphicsPipeline

-- compiled pipeline
data GLProgram
    = GLProgram
    { glpObject         :: GLuint
    , glpShaders        :: [GLuint]
    , glpAttributes     :: Trie (GLuint,AttributeType)
    , glpUniforms       :: Trie (GLint,UniformType)
    , glpSamplers       :: Trie (GLint,SamplerType,GLenum)
    , glpAttributeCount :: GLuint
    }

data GLAttribute
    = GLAttribute
    { glaType   :: AttributeType
    , glaSize   :: GLsizei  -- ^ attribute vector length
    , glaObject :: GLuint
    }

data GLPrimitive
    = GLPoints
    | GLTriangleStrip
    | GLTriangles
    | GLTriangleStripI  GLsizei GLuint -- ^ index buffer size, index buffer ID
    | GLTrianglesI      GLsizei GLuint

data GLMesh = GLMesh [(GLAttribute,ByteString)] GLPrimitive

class GLTexture2D t where
    glTexture2DObject :: t -> GLuint

data GLDepthTexture2D = GLDepthTexture2D GLuint
data GLStencilTexture = GLStencilTexture

data GLColorTexture2D = GLColorTexture2D GLuint
data GLFramebuffer = GLFramebuffer GLuint GLint

instance GLTexture2D GLColorTexture2D where
    glTexture2DObject (GLColorTexture2D to) = to

instance GLTexture2D GLDepthTexture2D where
    glTexture2DObject (GLDepthTexture2D to) = to


    {-
    -- shader
    uint CreateShader( enum type );
    void ShaderSource( uint shader, sizei count, const char**string,const int*length);
    void CompileShader( uint shader );
    void DeleteShader( uint shader );

    void GetShaderiv( uint shader, enum pname, int *params );
     - pname:
        SHADER_TYPE
            VERTEX_SHADER
            GEOMETRY_SHADER
            FRAGMENT_SHADER
        DELETE_STATUS
            TRUE
            FALSE
        COMPILE_STATUS
            TRUE
            FALSE
        INFO_LOG_LENGTH
        SHADER_SOURCE_LENGTH

    void GetShaderInfoLog( uint shader, sizei bufSize, sizei *length, char *infoLog );

    -- program
    uint CreateProgram( void );
    void AttachShader( uint program, uint shader );
    void DetachShader( uint program, uint shader );
    void LinkProgram( uint program );

    void GetProgramiv( uint program, enum pname, int *params );
     - pname:
        DELETE_STATUS
            TRUE
            FALSE
        LINK_STATUS
            TRUE
            FALSE
        VALIDATE_STATUS
            TRUE
            FALSE
        INFO_LOG_LENGTH
        ATTACHED_SHADERS
        ACTIVE_ATTRIBUTES
        ACTIVE_ATTRIBUTE_MAX_LENGTH
        ACTIVE_UNIFORMS
        ACTIVE_UNIFORM_MAX_LENGTH
        TRANSFORM_FEEDBACK_BUFFER_MODE
            SEPARATE_ATTRIBS
            INTERLEAVED_ATTRIBS
        TRANSFORM_FEEDBACK_VARYINGS
        TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
        ACTIVE_UNIFORM_BLOCKS
        ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH
        GEOMETRY_VERTICES_OUT
        GEOMETRY_INPUT_TYPE
            POINTS
            LINES
            LINES_ADJACENCY
            TRIANGLES
            TRIANGLES_ADJACENCY
        GEOMETRY_OUTPUT_TYPE
            POINTS
            LINE_STRIP
            TRIANGLE_STRIP

    void ValidateProgram( uint program );
    void GetProgramInfoLog( uint program, sizei bufSize, sizei *length, char *infoLog );

    void UseProgram( uint program );
    void DeleteProgram( uint program );

    -- vertex attributes
     -- queries:
    void GetActiveAttrib( uint program, uint index, sizei bufSize, sizei *length, int *size, enum *type, char *name );
    int GetAttribLocation( uint program, const char *name );
     -- setup:
    void BindAttribLocation( uint program, uint index, const char *name );

    -- uniform variables
    int GetUniformLocation( uint program, const char *name );
    uint GetUniformBlockIndex( uint program, const char *uniformBlockName );
    void GetActiveUniformBlockName( uint program, uint uniformBlockIndex, sizei bufSize, sizei *length, char *uniformBlockName );
    void GetActiveUniformBlockiv( uint program, uint uniformBlockIndex, enum pname, int *params );
     - pname
        UNIFORM_BLOCK_BINDING
        UNIFORM_BLOCK_DATA_SIZE
        UNIFORM_BLOCK_NAME_LENGTH
        UNIFORM_BLOCK_ACTIVE_UNIFORMS
        UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES
        UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER
        UNIFORM_BLOCK_REFERENCED_BY_GEOMETRY_SHADER
        UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER
    void GetUniformIndices( uint program, sizeiuniformCount,const char**uniformNames, uint *uniformIndices );
    void GetActiveUniformName( uint program, uint uniformIndex, sizei bufSize, sizei *length, char *uniformName );
    void GetActiveUniform( uint program, uint index, sizei bufSize, sizei *length, int *size, enum *type, char *name );
    void GetActiveUniformsiv( uint program, sizeiuniformCount,const uint*uniformIndices, enum pname, int *params );
     - pname
        UNIFORM_TYPE
        UNIFORM_SIZE
        UNIFORM_NAME_LENGTH
        UNIFORM_BLOCK_INDEX
        UNIFORM_OFFSET
        UNIFORM_ARRAY_STRIDE
        UNIFORM_MATRIX_STRIDE
        UNIFORM_IS_ROW_MAJOR
    void Uniform{1234}{if}( int location, T value );
    void Uniform{1234}{if}v( int location, sizei count, const T value );
    void Uniform{1234}ui( int location, T value );
    void Uniform{1234}uiv( int location, sizei count, const T value );
    void UniformMatrix{234}fv( int location, sizei count, booleantranspose,const float*value);
    void UniformMatrix{2x3,3x2,2x4,4x2,3x4,4x3}fv( int location, sizei count, boolean transpose, const float *value );

    -- uniform blocks
    void UniformBlockBinding( uint program, uint uniformBlockIndex, uint uniformBlockBinding );

    -- transform feedback
    void TransformFeedbackVaryings( uint program, sizeicount,const char**varyings,enumbufferMode);
    void GetTransformFeedbackVarying( uint program, uint index, sizei bufSize, sizei *length, sizei *size, enum *type, char *name );

    -}

printShaderLog :: GLuint -> IO ()
printShaderLog o = do
    i <- glGetShaderiv1 gl_INFO_LOG_LENGTH o
    allocaArray (fromIntegral i) $ \ps -> glGetShaderInfoLog o (fromIntegral i) nullPtr ps >> SB.packCString (castPtr ps) >>= SB.putStr

compileShader' :: GLuint -> [ByteString] -> IO ()
compileShader' o srcl = withMany SB.useAsCString srcl $ \l -> withArray l $ \p -> do
    glShaderSource o (fromIntegral $ length srcl) (castPtr p) nullPtr
    glCompileShader o
    printShaderLog o

glGetShaderiv1 :: GLenum -> GLuint -> IO GLint
glGetShaderiv1 pname o = alloca $ \pi -> glGetShaderiv o pname pi >> peek pi

glGetProgramiv1 :: GLenum -> GLuint -> IO GLint
glGetProgramiv1 pname o = alloca $ \pi -> glGetProgramiv o pname pi >> peek pi

printProgramLog :: GLuint -> IO ()
printProgramLog o = do
    i <- glGetProgramiv1 gl_INFO_LOG_LENGTH o
    allocaArray (fromIntegral i) $ \ps -> glGetProgramInfoLog o (fromIntegral i) nullPtr ps >> SB.packCString (castPtr ps) >>= SB.putStr

finalizeProgram :: GLProgram -> IO ()
finalizeProgram (GLProgram po l _ _ _ _) = glDeleteProgram po >> mapM_ glDeleteShader l

compileProgram' :: Program -> IO (Maybe GLProgram)
compileProgram' p = do
    po <- glCreateProgram
    let createAndAttach f t
            | null $ f p = return Nothing
            | otherwise = do
                o <- glCreateShader t
                compileShader o $ f p
                glAttachShader po o
                return $ Just o
    vs <- createAndAttach vertexShader gl_VERTEX_SHADER
    gs <- createAndAttach geometryShader gl_GEOMETRY_SHADER
    fs <- createAndAttach fragmentShader gl_FRAGMENT_SHADER
    glLinkProgram po
    printProgramLog po
    validateAndCreateGLProgram p po $ catMaybes [vs,gs,fs]

validateAndCreateGLProgram :: Program -> GLuint -> [GLuint] -> IO (Maybe GLProgram)
validateAndCreateGLProgram p po ol = do
    shstatus <- mapM (glGetShaderiv1 gl_COMPILE_STATUS) ol
    pstatus <- glGetProgramiv1 gl_LINK_STATUS po
    let failed = glDeleteProgram po >> mapM_ glDeleteShader ol >> return Nothing
    if any (/= fromIntegral gl_TRUE) (pstatus:shstatus) then failed else do
        (sl,ul) <- filterSamplers <$> getNameTypeSize po glGetActiveUniform glGetUniformLocation gl_ACTIVE_UNIFORMS gl_ACTIVE_UNIFORM_MAX_LENGTH
        al <- getNameTypeSize po glGetActiveAttrib glGetAttribLocation gl_ACTIVE_ATTRIBUTES gl_ACTIVE_ATTRIBUTE_MAX_LENGTH
        let ul' = [(n,(i,t)) | (n,i,e,s) <- ul, Just t <- [fromGLUniformType (e,s)]]
            al' = [(n,(fromIntegral i,t)) | (n,i,e,s) <- al, Just t <- [fromGLAttributeType (e,s)]]
            sl' = [(n,(i,t,tex)) | ((n,i,e,s),tex) <- zip sl [0..], Just t <- [fromGLSamplerType (e,s)]]
            lal = length al
            lul = length ul
            lsl = length sl
            okL = Set.size sA' == lal && Set.size sU' == lul && Set.size sS' == lsl
            sA  = Set.fromList $ attributes p
            sA' = Set.fromList [(n,t) | (n,(_,t)) <- al']
            sU  = Set.fromList $ uniforms p
            sU' = Set.fromList [(n,t) | (n,(_,t)) <- ul']
            sS  = Set.fromList $ samplers p
            sS' = Set.fromList [(n,t) | (n,(_,t,_)) <- sl']
        if sA == sA' && sU == sU' && sS == sS' && okL then return $ Just $ GLProgram po ol (T.fromList al') (T.fromList ul') (T.fromList sl') (fromIntegral lal) else do
            SB.putStrLn "[E] program validation failed:"
            SB.putStr " * missing attributes:  "  >> print (sA' Set.\\ sA)
            SB.putStr " * unbinded attributes: "  >> print (sA Set.\\ sA')
            SB.putStr " * missing uniforms:  "  >> print (sU' Set.\\ sU)
            SB.putStr " * unbinded uniforms: "  >> print (sU Set.\\ sU')
            SB.putStr " * missing samplers:  "  >> print (sS' Set.\\ sS)
            SB.putStr " * unbinded samplers: "  >> print (sS Set.\\ sS')
            failed

getNameTypeSize' :: GLuint -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
                   -> (GLuint -> Ptr GLchar -> IO GLint) -> GLenum -> GLenum -> IO [(ByteString,GLint,GLenum,GLint)]
getNameTypeSize' o f g enum enumLen = do
    nameLen <- glGetProgramiv1 enumLen o
    allocaArray (fromIntegral nameLen) $ \namep -> alloca $ \sizep -> alloca $ \typep -> do
        n <- glGetProgramiv1 enum o
        forM [0..n-1] $ \i -> f o (fromIntegral i) (fromIntegral nameLen) nullPtr sizep typep namep >>
            (,,,) <$> SB.packCString (castPtr namep) <*> g o namep <*> peek typep <*> peek sizep

filterSamplers :: [(ByteString,GLint,GLenum,GLint)] -> ([(ByteString,GLint,GLenum,GLint)],[(ByteString,GLint,GLenum,GLint)])
filterSamplers l = partition (\(_,_,e,_) -> elem e samplerTypes) l
  where
    samplerTypes = [gl_SAMPLER_2D]

fromGLSamplerType :: (GLenum,GLint) -> Maybe SamplerType
fromGLSamplerType (t,1)
    | t == gl_SAMPLER_2D = Just ST_Sampler2D
    | otherwise = Nothing
fromGLSamplerType _ = Nothing

fromGLUniformType :: (GLenum,GLint) -> Maybe UniformType
fromGLUniformType (t,1)
    | t == gl_FLOAT         = Just UT_Float
    | t == gl_FLOAT_VEC2    = Just UT_Vec2
    | t == gl_FLOAT_VEC3    = Just UT_Vec3
    | t == gl_FLOAT_VEC4    = Just UT_Vec4
    | t == gl_FLOAT_MAT2    = Just UT_Mat2
    | t == gl_FLOAT_MAT3    = Just UT_Mat3
    | t == gl_FLOAT_MAT4    = Just UT_Mat4
    | t == gl_INT           = Just UT_Int
    | t == gl_UNSIGNED_INT  = Just UT_Word
    | t == gl_BOOL          = Just UT_Bool
    | otherwise = Nothing
fromGLUniformType _ = Nothing

fromGLAttributeType :: (GLenum,GLint) -> Maybe AttributeType
fromGLAttributeType (t,1)
    | t == gl_FLOAT         = Just AT_Float
    | t == gl_FLOAT_VEC2    = Just AT_Vec2
    | t == gl_FLOAT_VEC3    = Just AT_Vec3
    | t == gl_FLOAT_VEC4    = Just AT_Vec4
    | t == gl_FLOAT_MAT2    = Just AT_Mat2
    | t == gl_FLOAT_MAT3    = Just AT_Mat3
    | t == gl_FLOAT_MAT4    = Just AT_Mat4
    | t == gl_INT           = Just AT_Int
    | t == gl_UNSIGNED_INT  = Just AT_Word
    | otherwise = Nothing
fromGLAttributeType _ = Nothing

uploadVector' :: Storable a => GLenum -> V.Vector a -> IO GLuint
uploadVector' t v = withForeignPtr fp $ \p -> do
    bo <- alloca $ \pbo -> glGenBuffers 1 pbo >> peek pbo
    glBindBuffer t bo
    glBufferData t (fromIntegral len) (plusPtr p offset) gl_STATIC_DRAW
    glBindBuffer t 0
    return bo
  where
    s = sizeOf $ v `V.unsafeIndex` 0
    (fp,o,l) = V.unsafeToForeignPtr v
    offset = o * s
    len = l * s

uploadVectorArray' :: Storable a => V.Vector a -> IO GLuint
uploadVectorArray' v = uploadVector gl_ARRAY_BUFFER v

attrSize :: Storable a => V.Vector a -> GLsizei
attrSize = fromIntegral . V.length

compileAttribute' :: Attribute -> IO GLAttribute
compileAttribute' (A_Float v)  = GLAttribute AT_Float (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Vec2 v)   = GLAttribute AT_Vec2  (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Vec3 v)   = GLAttribute AT_Vec3  (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Vec4 v)   = GLAttribute AT_Vec4  (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Mat2 v)   = GLAttribute AT_Mat2  (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Mat3 v)   = GLAttribute AT_Mat3  (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Mat4 v)   = GLAttribute AT_Mat4  (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Int v)    = GLAttribute AT_Int   (attrSize v) <$> uploadVectorArray v
compileAttribute' (A_Word v)   = GLAttribute AT_Word  (attrSize v) <$> uploadVectorArray v

finalizeAttribute :: GLAttribute -> IO ()
finalizeAttribute a = with (glaObject a) (glDeleteBuffers 1)

uploadVectorElement' :: V.Vector Int -> IO GLuint
uploadVectorElement' v = uploadVector gl_ELEMENT_ARRAY_BUFFER $ V.map (fromIntegral :: Int -> Int32) v

compilePrimitive' :: Primitive -> IO GLPrimitive
compilePrimitive' Points             = return GLPoints
compilePrimitive' TriangleStrip      = return GLTriangleStrip
compilePrimitive' Triangles          = return GLTriangles
compilePrimitive' (TriangleStripI v) = GLTriangleStripI (fromIntegral $ V.length v) <$> uploadVectorElement v
compilePrimitive' (TrianglesI v)     = GLTrianglesI (fromIntegral $ V.length v) <$> uploadVectorElement v

{-
    void Uniform{1234}{if}( int location, T value );
    void Uniform{1234}{if}v( int location, sizei count, const T value );
    void Uniform{1234}ui( int location, T value );
    void Uniform{1234}uiv( int location, sizei count, const T value );
    void UniformMatrix{234}fv( int location, sizei count, boolean transpose, const float *value);
    void UniformMatrix{2x3,3x2,2x4,4x2,3x4,4x3}fv( int location, sizei count, boolean transpose, const float *value );
-}
setUniform' :: GLProgram -> ByteString -> Uniform -> IO Bool
setUniform' p name (U_Float d) = checkUniform name p UT_Float $ \i -> with d $ \p -> glUniform1fv i 1 $ castPtr p
setUniform' p name (U_Vec2 d)  = checkUniform name p UT_Vec2  $ \i -> with d $ \p -> glUniform2fv i 1 $ castPtr p
setUniform' p name (U_Vec3 d)  = checkUniform name p UT_Vec3  $ \i -> with d $ \p -> glUniform3fv i 1 $ castPtr p
setUniform' p name (U_Vec4 d)  = checkUniform name p UT_Vec4  $ \i -> with d $ \p -> glUniform4fv i 1 $ castPtr p
setUniform' p name (U_Int d)   = checkUniform name p UT_Int   $ \i -> with d $ \p -> glUniform1iv i 1 $ castPtr p
setUniform' p name (U_Word d)  = checkUniform name p UT_Word  $ \i -> with d $ \p -> glUniform1uiv i 1 $ castPtr p
setUniform' p name (U_Bool d)  = checkUniform name p UT_Bool  $ \i -> glUniform1ui i (if d then 1 else 0)
setUniform' p name (U_Mat2 d)  = checkUniform name p UT_Mat2  $ \i -> with d $ \p -> glUniformMatrix2fv i 1 (fromIntegral gl_TRUE) $ castPtr p
setUniform' p name (U_Mat3 d)  = checkUniform name p UT_Mat3  $ \i -> with d $ \p -> glUniformMatrix3fv i 1 (fromIntegral gl_TRUE) $ castPtr p
setUniform' p name (U_Mat4 d)  = checkUniform name p UT_Mat4  $ \i -> with d $ \p -> glUniformMatrix4fv i 1 (fromIntegral gl_TRUE) $ castPtr p
--setUniform _ _ _ = return False

checkUniform' :: ByteString -> GLProgram -> UniformType -> (GLint -> IO ()) -> IO Bool
checkUniform' name p t f = case T.lookup name $ glpUniforms p of
    Nothing -> return False
    Just (i,t') -> if t /= t' then return False else f (fromIntegral i) >> return True

setSampler' :: GLTexture2D t => GLProgram -> ByteString -> t -> IO Bool
setSampler' p name t = checkSampler name p ST_Sampler2D $ \i tc -> do
    glUniform1i i (fromIntegral tc)
    glActiveTexture $ gl_TEXTURE0 + tc
    glBindTexture gl_TEXTURE_2D $ glTexture2DObject t

checkSampler' :: ByteString -> GLProgram -> SamplerType -> (GLint -> GLenum -> IO ()) -> IO Bool
checkSampler' name p t f = case T.lookup name $ glpSamplers p of
    Nothing -> return False
    Just (i,t',tc) -> if t /= t' then return False else f (fromIntegral i) tc >> return True

{-
-- buffer objects
void GenBuffers( sizei n, uint *buffers );
void DeleteBuffers( sizei n, const uint *buffers );
void BindBuffer( enum target, uint buffer );
 - target
    ARRAY_BUFFER
    COPY_READ_BUFFER
    COPY_WRITE_BUFFER
    ELEMENT_ARRAY_BUFFER
    PIXEL_PACK_BUFFER
    PIXEL_UNPACK_BUFFER
    TEXTURE_BUFFER
    TRANSFORM_FEEDBACK_BUFFER
    UNIFORM_BUFFER

void BindBufferRange( enum target, uint index, uint buffer, intptr offset, sizeiptr size );
void BindBufferBase( enum target, uint index, uint buffer );
 - target
    TRANSFORM_FEEDBACK_BUFFER
    UNIFORM_BUFFER

void BufferData( enum target, sizeiptr size, const void *data, enum usage );
 - usage
    STREAM_DRAW
    STREAM_READ
    STREAM_COPY
    STATIC_DRAW
    STATIC_READ
    STATIC_COPY
    DYNAMIC_DRAW
    DYNAMIC_READ
    DYNAMIC_COPY

void BufferSubData( enum target, intptr offset, sizeiptrsize,const void*data);

void *MapBufferRange( enum target, intptr offset, sizeiptr length, bitfield access );
 - access
    MAP_READ_BIT
    MAP_WRITE_BIT
    MAP_INVALIDATE_RANGE_BIT
void *MapBuffer( enum target, enum access );
void FlushMappedBufferRange( enum target, intptr offset, sizeiptr length );
boolean UnmapBuffer( enum target );
void *CopyBufferSubData( enum readtarget, enum writetarget, intptr readoffset, intptr writeoffset, sizeiptr size );

-- vertex array objects
void GenVertexArrays( sizei n, uint *arrays );
void DeleteVertexArrays( sizei n, const uint *arrays );
void BindVertexArray( uint array );

-}


    {-
    -- generic attributes
    void VertexAttrib{1234}{sfd}( uint index, T values );
    void VertexAttrib{123}{sfd}v( uint index, const T values );
    void VertexAttrib4{bsifd ub us ui}v( uint index, const T values );
    void VertexAttrib4Nub( uint index, T values );
    void VertexAttrib4N{bsi ub us ui}v( uint index, const T values );

    void VertexAttribI{1234}{i ui}( uint index, T values );
    void VertexAttribI{1234}{i ui}v( uint index, const T values );
    void VertexAttribI4{bs ubus}v( uint index, const T values );

    void VertexAttribP{1234}ui(uint index,enum type,boolean normalized,uint value);
    void VertexAttribP{1234}uiv(uint index,enum type,boolean normalized,const uint *value);

    -- vertex arrays
    void VertexAttribPointer( uint index, int size, enum type, boolean normalized, sizei stride, const void *pointer );
    void VertexAttribIPointer( uint index, int size, enum type, sizei stride, const void *pointer);

    void EnableVertexAttribArray( uint index );
    void DisableVertexAttribArray( uint index );

    void VertexAttribDivisor( uint index, uint divisor );

    -}

setAttribute' :: GLProgram -> GLAttribute -> ByteString -> IO Bool
setAttribute' p (GLAttribute a _ bo) name
    | a == AT_Float = setFloat 1
    | a == AT_Vec2  = setFloat 2
    | a == AT_Vec3  = setFloat 3
    | a == AT_Vec4  = setFloat 4
    | a == AT_Mat2  = setFloat 4
    | a == AT_Mat3  = setFloat 9
    | a == AT_Mat4  = setFloat 16
    | a == AT_Int   = setInt 1 gl_INT
    | a == AT_Word  = setInt 1 gl_UNSIGNED_INT
    | otherwise = return False
  where
    setFloat n = check $ \i -> glVertexAttribPointer i n gl_FLOAT (fromIntegral gl_FALSE) 0 nullPtr
    setInt n t = check $ \i -> glVertexAttribIPointer i n t 0 nullPtr
    check f    = case T.lookup name $ glpAttributes p of
        Nothing -> return False
        Just (i,t) -> if a /= t then return False else do
            glBindBuffer gl_ARRAY_BUFFER bo
            glEnableVertexAttribArray i
            f i
            glBindBuffer gl_ARRAY_BUFFER 0
            return True

withGLProgram' :: GLProgram -> IO a -> IO a
withGLProgram' p f = do
    glUseProgram (glpObject p)
    a <- f
    --glValidateProgram (glpObject p)
    --printProgramLog (glpObject p)
    glUseProgram 0
    return a

withGLAttributes' :: GLProgram -> [(GLAttribute,ByteString)] -> IO () -> IO Bool
withGLAttributes' p al f = do
    ok <- and <$> mapM (uncurry $ setAttribute p) al
    when ok f
    forM_ [0..glpAttributeCount p-1] glDisableVertexAttribArray
    return ok

    {-
    helper: void DrawArraysOneInstance( enum mode, int first, sizei count, int instance );

    void DrawArrays( enum mode, int first, sizei count );
    void DrawArraysInstanced( enum mode, int first, sizei count, sizei primcount );
    void MultiDrawArrays( enum mode, const int *first, const sizei *count, sizei primcount);

    helper: void DrawElementsOneInstance( enum mode, sizei count, enum type, const void *indices);

    void DrawElements( enum mode, sizei count, enum type, const void*indices);
    void DrawElementsInstanced( enum mode, sizei count, enum type, const void *indices, sizei primcount);
    void MultiDrawElements( enum mode, const sizei *count, enum type, const void **indices, sizei primcount );
    void DrawRangeElements( enum mode, uint start, uint end, sizei count, enum type, const void *indices );

    void DrawElementsBaseVertex( enum mode, sizei count, enum type, const void *indices, int basevertex);
    void DrawRangeElementsBaseVertex( enum mode, uint start, uint end, sizei count, enum type, const void *indices, int basevertex );
    void DrawElementsInstancedBaseVertex( enum mode, sizei count, enum type, const void *indices, sizei primcount, int basevertex );
    void MultiDrawElementsBaseVertex( enum mode, const sizei *count, enum type, const void **indices, sizei primcount, const int *basevertex);
    -}
renderGLPrimitive' :: GLPrimitive -> GLsizei -> IO ()
renderGLPrimitive' GLPoints s        = glDrawArrays gl_POINTS 0 s
renderGLPrimitive' GLTriangleStrip s = glDrawArrays gl_TRIANGLE_STRIP 0 s
renderGLPrimitive' GLTriangles s     = glDrawArrays gl_TRIANGLES 0 s
renderGLPrimitive' (GLTriangleStripI s bo) _ = glBindBuffer gl_ELEMENT_ARRAY_BUFFER bo >> glDrawElements gl_TRIANGLE_STRIP s gl_UNSIGNED_INT nullPtr
renderGLPrimitive' (GLTrianglesI s bo) _     = glBindBuffer gl_ELEMENT_ARRAY_BUFFER bo >> glDrawElements gl_TRIANGLES s gl_UNSIGNED_INT nullPtr

-- mesh utility functions
compileMesh' :: Mesh -> IO GLMesh
compileMesh' (Mesh al p) = GLMesh <$> mapM (\(n,a) -> (,n) <$> compileAttribute a) al <*> compilePrimitive p

renderGLMeshD :: GLProgram -> GLMesh -> IO Bool
renderGLMeshD p m = renderGLMesh' p m $ return True

-- this is for setting up uniforms in action
renderGLMesh'D :: GLProgram -> GLMesh -> IO Bool -> IO Bool
renderGLMesh'D p (GLMesh al pr) setup = withGLProgram p $ do
    ok <- setup
    if not ok then return False else
        withGLAttributes p [a | a@(_,name) <- al, hasAttr name] $ renderGLPrimitive pr s
  where
    s = glaSize $ fst $ head al
    hasAttr n = T.member n (glpAttributes p)

clearGLFramebuffer :: IO ()
clearGLFramebuffer = do
    glClearColor 0 0 0 1
    glClear $ fromIntegral (gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT)

-- texturing
{-
TEXTURE_1D
TEXTURE_2D
TEXTURE_3D
TEXTURE_1D_ARRAY
TEXTURE_2D_ARRAY
TEXTURE_RECTANGLE
TEXTURE_BUFFER
TEXTURE_CUBE_MAP
TEXTURE_2D_MULTISAMPLE
TEXTURE_2D_MULTISAMPLE_ARRAY
-}
{-
void SamplerParameter{if}v( uint sampler, enum pname, T param );
void SamplerParameterI{u ui}v( uint sampler, enum pname, T *params );
 - pname:
    TEXTURE_WRAP_S
    TEXTURE_WRAP_T
    TEXTURE_WRAP_R
    TEXTURE_MIN_FILTER
    TEXTURE_MAG_FILTER
    TEXTURE_BORDER_COLOR
    TEXTURE_MIN_LOD
    TEXTURE_MAX_LOD
    TEXTURE_LOD_BIAS
    TEXTURE_COMPARE_MODE
    TEXTURE_COMPARE_FUNC
-}

uploadTexture2D' :: Size -> GLenum -> GLenum -> Ptr a -> IO GLuint
uploadTexture2D' (w,h) intf f ptr = do
    glBindBuffer gl_ARRAY_BUFFER 0
    glBindBuffer gl_ELEMENT_ARRAY_BUFFER 0
    to <- alloca $ \pto -> glGenTextures 1 pto >> peek pto
    glBindTexture gl_TEXTURE_2D to
    --glPixelStorei gl_UNPACK_ALIGNMENT padding
    -- UNPACK_ROW_LENGTH and UNPACK_ALIGNMENT
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_BASE_LEVEL 0
    glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAX_LEVEL 0
    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral intf) (fromIntegral w) (fromIntegral h) 0 f gl_UNSIGNED_BYTE ptr
    glBindTexture gl_TEXTURE_2D 0
    return to

compileColorTexture2D' :: Bitmap Word8 -> IO GLColorTexture2D
compileColorTexture2D' b = withBitmap b $ \size 3 padding ptr -> GLColorTexture2D <$> uploadTexture2D size gl_RGBA32F gl_RGB ptr

newGLDepthTexture2D' :: Size -> IO GLDepthTexture2D
newGLDepthTexture2D' size = GLDepthTexture2D <$> uploadTexture2D size gl_DEPTH_COMPONENT32F gl_DEPTH_COMPONENT nullPtr
{-
COLOR_ATTACHMENT0
DEPTH_ATTACHMENT.
STENCIL_ATTACHMENT.
MAX_COLOR_ATTACHMENTS
-}
glGetIntegerv1 :: GLenum -> IO GLint
glGetIntegerv1 e = alloca $ \pi -> glGetIntegerv e pi >> peek pi

newGLFramebuffer :: IO GLFramebuffer
newGLFramebuffer = GLFramebuffer <$> (alloca $ \pto -> glGenFramebuffers 1 pto >> peek pto) <*> glGetIntegerv1 gl_MAX_COLOR_ATTACHMENTS

withFramebuffer' :: GLFramebuffer -> Maybe GLDepthTexture2D -> Maybe GLStencilTexture -> [GLColorTexture2D] -> IO Bool -> IO Bool
withFramebuffer' (GLFramebuffer fbo maxcolor) mdt Nothing cl a = if length cl > fromIntegral maxcolor then return False else do
    glBindFramebuffer gl_DRAW_FRAMEBUFFER fbo
    forM_ (zip [0 :: Int ..] cl) $ \(i,(GLColorTexture2D to)) ->
        glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D to 0
    -- disable unused
    forM_ [(fromIntegral $ length cl)..maxcolor-1] $ \i ->
        glFramebufferTexture2D gl_DRAW_FRAMEBUFFER (gl_COLOR_ATTACHMENT0 + fromIntegral i) gl_TEXTURE_2D 0 0
    let dto = case mdt of
            Nothing -> 0
            Just (GLDepthTexture2D to) -> to
    glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_DEPTH_STENCIL_ATTACHMENT gl_TEXTURE_2D 0 0
    glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_DEPTH_ATTACHMENT gl_TEXTURE_2D dto 0
    glFramebufferTexture2D gl_DRAW_FRAMEBUFFER gl_STENCIL_ATTACHMENT gl_TEXTURE_2D 0 0
    e <- glCheckFramebufferStatus gl_DRAW_FRAMEBUFFER
    b <- if e == gl_FRAMEBUFFER_COMPLETE then a else case e of
        0 -> checkGL >>= SB.putStrLn >> return False
        _ -> SB.putStrLn "Framebuffer is not complete" >> return False
    glBindFramebuffer gl_DRAW_FRAMEBUFFER 0
    return b

withDepth' :: IO a -> IO a
withDepth' a = do glEnable gl_DEPTH_TEST ; r <- a ; glDisable gl_DEPTH_TEST ; return r
{-
-- constraints
MAX_3D_TEXTURE_SIZE
MAX_ARRAY_TEXTURE_LAYERS
MAX_COLOR_ATTACHMENTS
MAX_COLOR_TEXTURE_SAMPLES
MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS
MAX_COMBINED_TEXTURE_IMAGE_UNITS
MAX_COMBINED_UNIFORM_BLOCKS
MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS
MAX_CUBE_MAP_TEXTURE_SIZE
MAX_DEPTH_TEXTURE_SAMPLES
MAX_DRAW_BUFFERS
MAX_DUAL_SOURCE_DRAW_BUFFERS
MAX_ELEMENTS_INDICES
MAX_ELEMENTS_VERTICES
MAX_FRAGMENT_INPUT_COMPONENTS
MAX_FRAGMENT_UNIFORM_BLOCKS
MAX_FRAGMENT_UNIFORM_COMPONENTS
MAX_GEOMETRY_INPUT_COMPONENTS
MAX_GEOMETRY_OUTPUT_COMPONENTS
MAX_GEOMETRY_OUTPUT_VERTICES
MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS
MAX_GEOMETRY_UNIFORM_BLOCKS
MAX_GEOMETRY_UNIFORM_COMPONENTS
MAX_INTEGER_SAMPLES
MAX_PROGRAM_TEXEL_OFFSET
MAX_RECTANGLE_TEXTURE_SIZE
MAX_RENDERBUFFER_SIZE
MAX_SAMPLES
MAX_SAMPLE_MASK_WORDS
MAX_SERVER_WAIT_TIMEOUT
MAX_TEXTURE_BUFFER_SIZE
MAX_TEXTURE_IMAGE_UNITS
MAX_TEXTURE_LOD_BIAS
MAX_TEXTURE_SIZE
MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
MAX_UNIFORM_BLOCK_SIZE
MAX_UNIFORM_BUFFER_BINDINGS
MAX_VARYING_COMPONENTS
MAX_VERTEX_ATTRIBS
MAX_VERTEX_OUTPUT_COMPONENTS
MAX_VERTEX_TEXTURE_IMAGE_UNITS
MAX_VERTEX_UNIFORM_BLOCKS
MAX_VERTEX_UNIFORM_COMPONENTS
MAX_VIEWPORT_DIMS
-}

glDebug :: IO ()
glDebug = do
    putStr "gl_MAX_DRAW_BUFFERS: "
    print =<< glGetIntegerv1 gl_MAX_DRAW_BUFFERS
--    putStr "gl_MAX_DUAL_SOURCE_DRAW_BUFFERS"
--    print =<< glGetIntegerv1 gl_MAX_DUAL_SOURCE_DRAW_BUFFERS

printGLVersion :: IO ()
printGLVersion = glDebug >> getString gl_VERSION >>= SB.putStrLn

getString :: GLenum -> IO ByteString
getString n = glGetString n >>= maybeNullPtr (return "") (SB.packCString . castPtr)
  where
    maybeNullPtr :: b -> (Ptr a -> b) -> Ptr a -> b
    maybeNullPtr n f ptr | ptr == nullPtr = n
                         | otherwise      = f ptr

-----------------
-- debug code
-----------------
checkGL :: IO ByteString
checkGL = do
    let f e | e == gl_INVALID_ENUM = "INVALID_ENUM"
            | e == gl_INVALID_VALUE = "INVALID_VALUE"
            | e == gl_INVALID_OPERATION = "INVALID_OPERATION"
            | e == gl_INVALID_FRAMEBUFFER_OPERATION = "INVALID_FRAMEBUFFER_OPERATION"
            | e == gl_OUT_OF_MEMORY = "OUT_OF_MEMORY"
            | e == gl_NO_ERROR = "OK"
            | otherwise = "Unknown error"
    e <- glGetError
    return $ f e

st :: ByteString -> IO ()
st n = do
    s <- checkGL
    unless (s == "OK") $ do
        SB.putStr n
        SB.putStr " "
        SB.putStrLn s

renderGLMesh' :: GLProgram -> GLMesh -> IO Bool -> IO Bool
renderGLMesh' p m a = do
    st "BEGIN[renderGLMesh']"
    r <- renderGLMesh'D p m a
    st "END[renderGLMesh']"
    return r

renderGLMesh :: GLProgram -> GLMesh -> IO Bool
renderGLMesh p m = do
    st "BEGIN[renderGLMesh]"
    r <- renderGLMeshD p m
    st "END[renderGLMesh]"
    return r

compileMesh :: Mesh -> IO GLMesh
compileMesh m = do
    st "BEGIN[compileMesh]"
    r <- compileMesh' m
    st "END[compileMesh]"
    return r

renderGLPrimitive :: GLPrimitive -> GLsizei -> IO ()
renderGLPrimitive p s = do
    st "BEGIN[renderGLPrimitive]"
    r <- renderGLPrimitive' p s
    st "END[renderGLPrimitive]"
    return r

withGLAttributes :: GLProgram -> [(GLAttribute,ByteString)] -> IO () -> IO Bool
withGLAttributes p l a = do
    st "BEGIN[withGLAttributes]"
    r <- withGLAttributes' p l a
    st "END[withGLAttributes]"
    return r

withGLProgram :: GLProgram -> IO a -> IO a
withGLProgram p a = do
    st "BEGIN[withGLProgram]"
    r <- withGLProgram' p a
    st "END[withGLProgram]"
    return r

setAttribute :: GLProgram -> GLAttribute -> ByteString -> IO Bool
setAttribute p l a = do
    st "BEGIN[setAttribute]"
    r <- setAttribute' p l a
    st "END[setAttribute]"
    return r

checkUniform :: ByteString -> GLProgram -> UniformType -> (GLint -> IO ()) -> IO Bool
checkUniform n p t a = do
    st "BEGIN[checkUniform]"
    r <- checkUniform' n p t a
    st "END[checkUniform]"
    return r

setUniform :: GLProgram -> ByteString -> Uniform -> IO Bool
setUniform p n u = do
    st "BEGIN[setUniform]"
    r <- setUniform' p n u
    st "END[setUniform]"
    return r

checkSampler :: ByteString -> GLProgram -> SamplerType -> (GLint -> GLenum -> IO ()) -> IO Bool
checkSampler n p t a = do
    st "BEGIN[checkSampler]"
    r <- checkSampler' n p t a
    st "END[checkSampler]"
    return r

setSampler :: GLTexture2D t => GLProgram -> ByteString -> t -> IO Bool
setSampler p n u = do
    st "BEGIN[setSampler]"
    r <- setSampler' p n u
    st "END[setSampler]"
    return r

compileColorTexture2D :: Bitmap Word8 -> IO GLColorTexture2D
compileColorTexture2D b = do
    st "BEGIN[compileColorTexture2D]"
    r <- compileColorTexture2D' b
    st "END[compileColorTexture2D]"
    return r

compilePrimitive :: Primitive -> IO GLPrimitive
compilePrimitive p = do
    st "BEGIN[compilePrimitive]"
    r <- compilePrimitive' p
    st "END[compilePrimitive]"
    return r

uploadVectorElement :: V.Vector Int -> IO GLuint
uploadVectorElement v = do
    st "BEGIN[uploadVectorElement]"
    r <- uploadVectorElement' v
    st "END[uploadVectorElement]"
    return r

compileAttribute :: Attribute -> IO GLAttribute
compileAttribute a = do
    st "BEGIN[compileAttribute]"
    r <- compileAttribute' a
    st "END[compileAttribute]"
    return r

uploadVectorArray :: Storable a => V.Vector a -> IO GLuint
uploadVectorArray v = do
    st "BEGIN[uploadVectorArray]"
    r <- uploadVectorArray' v
    st "END[uploadVectorArray]"
    return r

uploadVector :: Storable a => GLenum -> V.Vector a -> IO GLuint
uploadVector e v = do
    st "BEGIN[uploadVector]"
    r <- uploadVector' e v
    st "END[uploadVector]"
    return r

getNameTypeSize :: GLuint -> (GLuint -> GLuint -> GLsizei -> Ptr GLsizei -> Ptr GLint -> Ptr GLenum -> Ptr GLchar -> IO ())
                   -> (GLuint -> Ptr GLchar -> IO GLint) -> GLenum -> GLenum -> IO [(ByteString,GLint,GLenum,GLint)]
getNameTypeSize a b c d e = do
    st "BEGIN[getNameTypeSize]"
    r <- getNameTypeSize' a b c d e
    st "END[getNameTypeSize]"
    return r

compileProgram :: Program -> IO (Maybe GLProgram)
compileProgram p = do
    st "BEGIN[compileProgram]"
    r <- compileProgram' p
    st "END[compileProgram]"
    return r

compileShader :: GLuint -> [ByteString] -> IO ()
compileShader a b = do
    st "BEGIN[compileShader]"
    r <- compileShader' a b
    st "END[compileShader]"
    return r

withFramebuffer :: GLFramebuffer -> Maybe GLDepthTexture2D -> Maybe GLStencilTexture -> [GLColorTexture2D] -> IO Bool -> IO Bool
withFramebuffer a b c d e = do
    st "BEGIN[withFramebuffer]"
    r <- withFramebuffer' a b c d e
    st "END[withFramebuffer]"
    return r

uploadTexture2D :: Size -> GLenum -> GLenum -> Ptr a -> IO GLuint
uploadTexture2D a b c d = do
    st "BEGIN[uploadTexture2D]"
    r <- uploadTexture2D' a b c d
    st "END[uploadTexture2D]"
    return r

newGLDepthTexture2D :: Size -> IO GLDepthTexture2D
newGLDepthTexture2D a = do
    st "BEGIN[newGLDepthTexture2D]"
    r <- newGLDepthTexture2D' a
    st "END[newGLDepthTexture2D]"
    return r

withDepth :: IO a -> IO a
withDepth a = do
    st "BEGIN[withDepth]"
    r <- withDepth' a
    st "END[withDepth]"
    return r
