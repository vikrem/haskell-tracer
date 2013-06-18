module Lib.Scene where
import Lib.Vector
import Lib.Image
import Lib.Shader

import Debug.Trace
bgcolour = [128,128,128]

type Scene = [Surface]
type ShaderFunc = Ray -> Surface -> Maybe Scalar
data Surface = Sphere {
						origin :: Vector,
						radius :: Scalar,
						colour :: Colour,
						intersectFunc :: ShaderFunc 
						}

data Ray = Ray { rayOrigin :: Vector, normDir :: Vector }
instance Show Ray where
	show r = "[RAY] Origin: " ++ show (rayOrigin r) ++ " Dir: " ++ show (normDir r)


epsilon :: Float
epsilon = 0.001
dbg x = trace (show x) x
cameraRay :: Scalar -> Scalar -> Ray
cameraRay x y = Ray { rayOrigin = cameraOrigin,
				  normDir = normalize distvec }
				  where
				  distvec = Vector [x, y, 1.0] - cameraOrigin
vecFwd = Vector [0, 0, 1]
fromDegrees deg = deg * pi / 180 
toDegrees   rad = rad * 180 / pi
testRay :: Ray -> Colour
testRay r = map (round . abs . (*) 255 . (^3)) $ replicate 3 col
			where
			col = dot (normalize $ normDir r) (normalize vecFwd) 
render :: Scene -> Integer -> Integer -> Image
render _ width height = (width, height, img)
				where
					img = [ testRay (cameraRay (screenToWorld lx width) (screenToWorld ly height)) |
							ly <- [0..height-1],
							lx <- [0..width-1]]
screenToWorld :: Integer -> Integer -> Scalar
screenToWorld screencoord limit = (fromIntegral screencoord) / (fromIntegral $ limit-1) 
