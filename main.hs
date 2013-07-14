import Lib.Vector
import Lib.Image
import Lib.Scene
import Lib.Math
import Lib.Prims
main :: IO ()

-- Shade based on direct fresnal facing value (dotprod between camera view and surface normal)
cameraShader :: Colour -> ShaderFunc
cameraShader diffuseColor hitpoint surf _ = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = abs $ dot (normalize diffvec) (normalize normal)
				diffvec = cameraOrigin - hitpoint
				normal = hitpoint - origin surf

-- Use the camera as a point light with inverse square law falloff
cameraLinFalloffShader :: Colour -> Scalar -> ShaderFunc
cameraLinFalloffShader diffuseColor luminosity hitpoint _ _ = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = min (luminosity / (sqr $ mag diffvec)) 1.0
				diffvec = cameraOrigin - hitpoint
				sqr x = x * x

-- Trace rays to light sources. If it works, illuminate. Otherwise, you're in shadow
-- The following code doesn't work. I don't know why just yet.
harshShader :: Colour -> ShaderFunc
harshShader diffuseColor hitpoint _ scn = case lightresult of
												True -> lightcolour
												False -> darkcolour
				where
				lightresult = isLightVisible hitpoint scn
				lightcolour = diffuseColor
				darkcolour = map ( round . ((/) 2). fromIntegral ) diffuseColor

isLightVisible org scn = length (filter reachable lights) > 0
			where
			reachable obj = case hitObjects scn (rayToObj obj) of
								Just (obj, _) -> True
								Nothing -> False
			rayToObj obj = Ray org (normalize $ (origin obj) - org)
			lights = filter ((==) Emit . surftype) scn

-- Direct, lightless pixel shader that is a solid uniform colour
emissionShader :: Colour -> ShaderFunc
emissionShader emitcolour _ _ _ = emitcolour

-- Make pretty list of spheres
makeSphereArray = map makeSphere positions
				where
				makeSphere org = (Surface Absorb org (sphereHit 0.20) (cameraLinFalloffShader [0,165,255] 6))
				positions = [ (Vector [xpos, ypos, zpos]) | xpos <- xrange,
												ypos <- yrange,
												zpos <- zrange ]
				xrange = [-1.0,-0.6..2.0] :: [Scalar]
				yrange = [1.1] :: [Scalar]
				zrange = [2.0..5.0] :: [Scalar]

-- List of objects in our scene
scene = [ 
		  (Surface Absorb (Vector [0.3, 0.8, 3]) (sphereHit 0.3) (cameraLinFalloffShader [0, 165, 255] 2 )),
		  (Surface Absorb (Vector [0.3, 1.5, 3]) (sphereHit 0.20) (cameraLinFalloffShader [125, 165, 255] 2 )),
		  (Surface Emit (Vector [0.3, 0.2, 3]) (sphereHit 0.20) (cameraLinFalloffShader [255, 255, 255] 2 ))
		  ]
-- Image info
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
prettyMain = writeFile "out.ppm" $ create_ppm $ render makeSphereArray width height
