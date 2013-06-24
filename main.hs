import Lib.Vector
import Lib.Image
import Lib.Scene
import Lib.Math
main :: IO ()

sphereHit :: IntersectFunc
sphereHit ray sphere = case qdformula (dot d d)
							 (dot (mul 2 (o - c)) d)
							 ((dot (o - c) (o - c)) - r^2) of
				   Just roots -> Just $ finalHit roots
				   Nothing -> Nothing
			  where
			  d = normDir ray
			  o = rayOrigin ray
			  r = radius sphere
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

gouradShader :: Colour -> ShaderFunc
gouradShader diffuseColor hitpoint surf = map ( round . ( (*) ratio) . fromIntegral  ) $ diffuseColor
				where
				ratio = abs $ dot (normalize diffvec) (normalize normal)
				diffvec = cameraOrigin - (origin surf)
				normal = hitpoint - origin surf

emissionShader :: Colour -> ShaderFunc
emissionShader emitcolour _ _ = emitcolour

scene = [ 
		  (Surface (Vector [0.7, 0.7, 2]) 0.25 sphereHit (emissionShader [255, 255, 255]))
		  ]
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
