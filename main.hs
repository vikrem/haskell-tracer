import Lib.Vector
import Lib.Image
import Lib.Scene
main :: IO ()

noShader :: ShaderFunc
noShader _ _ = Nothing

scene = [ (Sphere (Vector [1,1,10]) 1 [255,0,0] noShader) ] 
width = 1024
height = 768

main = writeFile "out.ppm" $ create_ppm $ render scene width height
