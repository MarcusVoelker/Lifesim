module Run (run) where

import Graphics.UI.GLUT
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import Import
import LoadShaders

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  mainOpenGL "Test"

bufferOffset :: Integral a => a -> Ptr b
bufferOffset = plusPtr nullPtr . fromIntegral

data Descriptor = Descriptor VertexArrayObject ArrayIndex NumArrayIndices

data ColoredVertex = ColoredVertex (Vertex2 GLfloat) (Color3 GLfloat)

instance Storable ColoredVertex where
  sizeOf ~(ColoredVertex v c) = sizeOf v + sizeOf c
  alignment ~(ColoredVertex v _) = alignment v
  peek ptr = do v <- peek (castPtr ptr)
                c <- peekByteOff ptr (sizeOf v)
                return $ ColoredVertex v c
  poke ptr (ColoredVertex v c) = do poke (castPtr ptr) v
                                    pokeByteOff ptr (sizeOf v) c

init :: HasLogFunc env => RIO env Descriptor
init = do
  triangles <- genObjectName
  bindVertexArrayObject $= Just triangles

  let vertices = [
        -- Triangle 1
        ColoredVertex (Vertex2 (-0.90) (-0.90)) (Color3 1 0 0),
        ColoredVertex (Vertex2   0.85  (-0.90)) (Color3 0 1 0),
        ColoredVertex (Vertex2 (-0.90)   0.85 ) (Color3 0 0 1),
        -- Triangle 2
        ColoredVertex (Vertex2   0.90  (-0.85)) (Color3 0 1 1),
        ColoredVertex (Vertex2   0.90    0.90 ) (Color3 1 0 1),
        ColoredVertex (Vertex2 (-0.85)   0.90 ) (Color3 1 1 0)]
      numVertices = length vertices
  vertexSize <- case vertices of
    (h : _) -> return (sizeOf h)
    _ -> logError "Empty vertices" >> return 0

  arrayBuffer <- genObjectName
  bindBuffer ArrayBuffer $= Just arrayBuffer
  liftIO $ withArray vertices $ \ptr -> do
    let size = fromIntegral (numVertices * vertexSize)
    bufferData ArrayBuffer $= (size, ptr, StaticDraw)

  program <- loadShaders [
     ShaderInfo VertexShader (FileSource "res/shader/color_triangles.vert"),
     ShaderInfo FragmentShader (FileSource "res/shader/color_triangles.frag")]
  currentProgram $= Just program

  let firstIndex = 0
      vPosition = AttribLocation 0
      vColor = AttribLocation 1
  vertexAttribPointer vPosition $=
    (ToFloat,
     VertexArrayDescriptor 2 Float (fromIntegral vertexSize)
                           (bufferOffset (firstIndex * vertexSize)))
  vertexAttribArray vPosition $= Enabled
  colorOffset <- case vertices of (~(ColoredVertex v _)):_ -> return $ sizeOf v; _ -> logError "Empty vertices!" >> return 0
  vertexAttribPointer vColor $=
    (ToFloat,
     VertexArrayDescriptor 3 Float (fromIntegral vertexSize)
                           (bufferOffset ((firstIndex * vertexSize) +
                                          fromIntegral colorOffset)))
  vertexAttribArray vColor $= Enabled

  return $
    Descriptor triangles (fromIntegral firstIndex) (fromIntegral numVertices)

dcb2 :: Descriptor -> DisplayCallback
dcb2 (Descriptor triangles firstIndex numVertices) = do
  clear [ ColorBuffer ]
  bindVertexArrayObject $= Just triangles
  drawArrays Triangles firstIndex numVertices
  flush

reshape :: ReshapeCallback
reshape s@(Size width height) = do
   let h = fromIntegral height / fromIntegral width

   viewport $= (Position 0 0, s)
   matrixMode $= Projection
   loadIdentity
   frustum (-1) 1 (-h) h 5 60
   matrixMode $= Modelview 0
   loadIdentity
   translate (Vector3 0 0 (-40 :: GLfloat))

mainOpenGL :: HasLogFunc env => String -> RIO env ()
mainOpenGL title = do
  (progName, _args) <- getArgsAndInitialize
  initialDisplayMode $= [ RGBAMode ]
  initialWindowSize $= Size 512 512
  initialContextVersion $= (4, 3)
  initialContextFlags $= [ DebugContext ]
  initialContextProfile $= [ CoreProfile ]
  _ <- createWindow title
  descriptor <- init
  displayCallback $= dcb2 descriptor
  reshapeCallback $= Just reshape
  mainLoop