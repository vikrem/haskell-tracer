import Lib.Vector
import Lib.Image
import Lib.Scene
import Lib.Math
main :: IO ()

sphereHit :: ShaderFunc
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

scene = [ (Sphere (Vector [1,1,10]) 1 [255,0,0] sphereHit) ] 
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
