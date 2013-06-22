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
sphereShader :: ShaderFunc
sphereShader hitpoint sphere = map ( round . ( (*) ratio) . fromIntegral  ) $ colour sphere
				where
				ratio = abs $ dot (normalize vecFwd) (normalize normal)
				normal = hitpoint - origin sphere

scene = [ (Sphere (Vector [0.5,0.5,10]) 1 [255,0,0] sphereHit sphereShader) ] 
width = 800
height = 800

main = writeFile "out.ppm" $ create_ppm $ render scene width height
