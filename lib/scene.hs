module Lib.Scene where
import Lib.Vector
import Lib.Image
import Lib.Shader
import Lib.Math
import Debug.Trace
bgcolour = [0,0,0]

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
testRay :: Scene -> Ray -> Colour
testRay s ray = case qdformula (dot d d)
							 (dot (mul 2 (o - c)) d)
							 ((dot (o - c) (o - c)) - r^2) of
				   Just _ -> [255,0,0]
				   Nothing -> bgcolour
			  where
			  d = normDir ray
			  o = rayOrigin ray
			  r = radius (head s)
			  c = origin (head s)
render :: Scene -> Integer -> Integer -> Image
render scene width height = (width, height, img)
				where
					img = [ testRay scene (cameraRay (screenToWorld lx width) (screenToWorld ly height)) |
							ly <- [0..height-1],
							lx <- [0..width-1]]
screenToWorld :: Integer -> Integer -> Scalar
screenToWorld screencoord limit = (fromIntegral screencoord) / (fromIntegral $ limit-1) 
