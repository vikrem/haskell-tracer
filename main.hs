import Lib.Vector
import Lib.Image
import Lib.Scene
import Lib.Math
main :: IO ()

noShader :: ShaderFunc
noShader _ _ = error "what no"

scene = [ (Sphere (Vector [1,1,10]) 1 [255,0,0] noShader) ] 
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
