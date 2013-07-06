-- Primitive math funcs for stuff
module Lib.Prims where

import Lib.Math
import Lib.Vector
import Lib.Scene


-- Determine if a ray has collided with a sphere of a given radius
sphereHit :: Scalar -> IntersectFunc
sphereHit radius ray sphere = case solution of
				   Just roots -> Just $ finalHit roots
				   Nothing -> Nothing
			  where
			  d = normDir ray
			  o = rayOrigin ray
			  r = radius
			  c = origin sphere
			  solution = qdformula (dot d d)
									(dot (mul 2 (o - c)) d)
									((dot (o - c) (o - c)) - r^2)
			  finalHit roots = rayOrigin ray + mul (best roots) (normDir ray) 
			  best = head . (filter (>0) )

-- Determine if a ray has collided with an infinite plane
-- Currently commented out due to lack of a good plane shader
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
