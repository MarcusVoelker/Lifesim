--------------------------------------------------------------------------------
-- |
-- Module      :  LoadShaders
-- Copyright   :  (c) Sven Panne 2018
-- License     :  BSD3
--
-- Maintainer  :  Sven Panne <svenpanne@gmail.com>
-- Stability   :  stable
-- Portability :  portable
--
-- Utilities for shader handling, adapted from LoadShaders.cpp which is (c) The
-- Red Book Authors.
--
--------------------------------------------------------------------------------

module LoadShaders (
   ShaderSource(..), ShaderInfo(..), loadShaders
) where

import RIO
import qualified RIO.ByteString as B
import Graphics.UI.GLUT

--------------------------------------------------------------------------------

-- | The source of the shader source code.

data ShaderSource =
     ByteStringSource B.ByteString
     -- ^ The shader source code is directly given as a 'B.ByteString'.
   | StringSource String
     -- ^ The shader source code is directly given as a 'String'.
   | FileSource FilePath
     -- ^ The shader source code is located in the file at the given 'FilePath'.
   deriving ( Eq, Ord, Show )

getSource :: ShaderSource -> IO B.ByteString
getSource (ByteStringSource bs) = return bs
getSource (StringSource str) = return $ packUtf8 str
getSource (FileSource path) = B.readFile path

--------------------------------------------------------------------------------

-- | A description of a shader: The type of the shader plus its source code.

data ShaderInfo = ShaderInfo ShaderType ShaderSource
   deriving ( Eq, Ord, Show )

--------------------------------------------------------------------------------

-- | Create a new program object from the given shaders, throwing an
-- 'IOException' if something goes wrong.

loadShaders :: HasLogFunc env => [ShaderInfo] -> RIO env Program
loadShaders infos = withRunInIO $ \run ->
   createProgram `bracketOnError` deleteObjectName $ \program -> do
      run $ loadCompileAttach program infos
      run $ linkAndCheck program
      return program

linkAndCheck :: HasLogFunc env => Program -> RIO env ()
linkAndCheck = checked (liftIO . linkProgram) linkStatus programInfoLog "link"

loadCompileAttach :: HasLogFunc env => Program -> [ShaderInfo] -> RIO env ()
loadCompileAttach _ [] = return ()
loadCompileAttach program (ShaderInfo shType source : infos) = withRunInIO $ \run ->
   createShader shType `bracketOnError` deleteObjectName $ \shader -> do
      src <- getSource source
      shaderSourceBS shader $= src
      run $ compileAndCheck shader
      attachShader program shader
      run $ loadCompileAttach program infos

compileAndCheck :: HasLogFunc env => Shader -> RIO env ()
compileAndCheck = checked (liftIO . compileShader) compileStatus shaderInfoLog "compile"

checked :: HasLogFunc env => (t -> RIO env ())
        -> (t -> GettableStateVar Bool)
        -> (t -> GettableStateVar String)
        -> String
        -> t
        -> RIO env ()
checked action getStatus getInfoLog message object = do
   action object
   ok <- get (getStatus object)
   unless ok $ do
      infoLog <- get (getInfoLog object)
      logError $ displayBytesUtf8 (fromString (message ++ " log: " ++ infoLog) :: B.ByteString)
