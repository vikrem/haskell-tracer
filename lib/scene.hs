module Lib.Scene where

import Data.Ord
import Data.List
import Data.Function

import Lib.Vector
import Lib.Image
import Lib.Shader
import Lib.Math
import Debug.Trace
bgcolour = [0,0,0]

type Scene = [Surface]
type IntersectFunc = Ray -> Surface -> Maybe Vector -- Return the hitpoint (or lack of) on a surface via a ray
type ShaderFunc = Vector -> Surface -> Colour -- Take in a hitpoint and return a colour
data ObjectType = Light | Object
data Surface = Surface {
						origin :: Vector,
						hit :: IntersectFunc,
						shader :: ShaderFunc
						}

data Ray = Ray { rayOrigin :: Vector, normDir :: Vector }
instance Show Ray where
	show r = "[RAY] Origin: " ++ show (rayOrigin r) ++ " Dir: " ++ show (normDir r)
dbg x = trace (show x) x
cameraRay :: Scalar -> Scalar -> Ray
cameraRay x y = Ray { rayOrigin = cameraOrigin,
				  normDir = normalize distvec }
				  where
				  distvec = Vector [x, y, 1.0] - cameraOrigin
vecFwd = Vector [0, 0, 1]
testRay :: Scene -> Ray -> Colour
testRay s r = case hitresult of
				Just hitpoint -> (shader obj) hitpoint obj
				Nothing -> bgcolour
			where
				(obj, hitresult) = minimumBy (hitCompare `on` snd) [ (obj, ((hit obj) r obj)) | obj <- s]
				hitCompare (Just x) (Just y) = compare x y
				hitCompare (Just x) Nothing = LT
				hitCompare Nothing (Just y) = GT
				hitCompare _ _ = EQ

render :: Scene -> Integer -> Integer -> Image
render scene width height = (width, height, img)
				where
					img = [ testRay scene (cameraRay (screenToWorld lx width) (screenToWorld ly height)) |
							ly <- [0..height-1],
							lx <- [0..width-1]]
screenToWorld :: Integer -> Integer -> Scalar
screenToWorld screencoord limit = (fromIntegral screencoord) / (fromIntegral $ limit-1) 
