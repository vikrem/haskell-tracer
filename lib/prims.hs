-- Primitive math funcs for stuff
import Lib.Math
import Lib.Shader
import Lib.Vector
import Lib.Scene


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
