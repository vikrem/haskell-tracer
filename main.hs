import Lib.Vector
import Lib.Image
import Lib.Scene
import Lib.Math
main :: IO ()

sphereHit :: Scalar -> IntersectFunc
sphereHit radius ray sphere = case qdformula (dot d d)
							 (dot (mul 2 (o - c)) d)
							 ((dot (o - c) (o - c)) - r^2) of
				   Just roots -> Just $ finalHit roots
				   Nothing -> Nothing
			  where
			  d = normDir ray
			  o = rayOrigin ray
			  r = radius
			  c = origin sphere
			  finalHit roots = rayOrigin ray + mul (best roots) (normDir ray) 
			  best = head . (filter (>0) )
{-
planeHit :: IntersectFunc
planeHit ray plane
				| fleq denom epsilon = Nothing
				| t < 0 = Nothing
				| otherwise = Just $ rayOrigin ray + mul t (normDir ray)
				where
					denom = dot (normDir ray) (normal plane)
					numer = dot ((origin plane) - (rayOrigin ray)) (normal plane)
					t = numer / denom
-}

cameraShader :: Colour -> ShaderFunc
cameraShader diffuseColor hitpoint surf = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = abs $ dot (normalize diffvec) (normalize normal)
				diffvec = cameraOrigin - hitpoint
				normal = hitpoint - origin surf

cameraLinFalloffShader :: Colour -> Scalar -> ShaderFunc
cameraLinFalloffShader diffuseColor luminosity hitpoint surf = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = min (luminosity / (sqr $ mag diffvec)) 1.0
				diffvec = cameraOrigin - hitpoint
				sqr x = x * x

emissionShader :: Colour -> ShaderFunc
emissionShader emitcolour _ _ = emitcolour

scene = [ 
		  (Surface (Vector [0.7, 0.7, 1]) (sphereHit 0.25) (cameraShader [0, 165, 255] )),
		  (Surface (Vector [0.3, 0.3, 2]) (sphereHit 0.25) (cameraShader [255, 165, 0] ))
		  ]
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
