import Lib.Vector
import Lib.Image
import Lib.Scene
import Lib.Math
import Lib.Prims

main :: IO ()

-- Shade based on direct fresnal facing value (dotprod between camera view and surface normal)
cameraShader :: Colour -> ShaderFunc
cameraShader diffuseColor hitpoint surf = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = abs $ dot (normalize diffvec) (normalize normal)
				diffvec = cameraOrigin - hitpoint
				normal = hitpoint - origin surf

-- Use the camera as a point light with inverse square law falloff
cameraLinFalloffShader :: Colour -> Scalar -> ShaderFunc
cameraLinFalloffShader diffuseColor luminosity hitpoint surf = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = min (luminosity / (sqr $ mag diffvec)) 1.0
				diffvec = cameraOrigin - hitpoint
				sqr x = x * x

-- Direct, lightless pixel shader that is a solid uniform colour
emissionShader :: Colour -> ShaderFunc
emissionShader emitcolour _ _ = emitcolour

-- List of objects in our scene
scene = [ 
		  (Surface (Vector [0.7, 0.7, 1]) (sphereHit 0.25) (cameraShader [0, 165, 255] )),
		  (Surface (Vector [0.3, 0.3, 2]) (sphereHit 0.25) (cameraShader [255, 165, 0] ))
		  ]
-- Image info
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
