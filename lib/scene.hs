module Lib.Scene where

import Data.Ord
import Data.List
import Data.Function
import Data.Monoid

import Lib.Vector
import Lib.Image
import Lib.Math
import Debug.Trace

-- Background colour for when rays don't hit anything
bgcolour = [0,0,0]

type Scene = [Surface]
type IntersectFunc = Ray -> Surface -> Maybe Vector -- Return the hitpoint (or lack of) on a surface via a ray
type ShaderFunc = Vector -> Surface -> Scene -> Colour -- Take in a hitpoint, the object, the scene, and return a colour
type Traceresult = Maybe (Surface, Colour) -- Result of a raytrace, the first hit object and it's colour from shading
data ObjectType = Emit | Absorb -- For doing light calculations later, we'll need to know which mesh are light and which are not
data Surface = Surface {
						surftype :: ObjectType, -- Does this object emit or absorb light?
						origin :: Vector, -- All surfaces have an origin
						hit :: IntersectFunc, -- A primitive-based ray-collision func found in prims.hs
						shader :: ShaderFunc -- A pixel/fragment shader
						}

-- Rays start from somwhere and proceed infinitely in a direction
data Ray = Ray { rayOrigin :: Vector, normDir :: Vector } deriving (Show, Eq)

-- Produce the camera ray (ray from our eye) through the viewport
-- With the viewing frustum spanning from 0.0 to 1.0
cameraRay :: Scalar -> Scalar -> Ray
cameraRay x y = Ray { rayOrigin = cameraOrigin,
				  normDir = normalize distvec }
				  where
				  distvec = Vector [x, y, 1.0] - cameraOrigin

getTraceColour :: Traceresult -> Colour
getTraceColour tr = case tr of
						Just (surf, col) -> col
						Nothing -> bgcolour

-- Gets the first hit object from the ray and returns the colour
getPixelForRay :: Scene -> Ray -> Colour
getPixelForRay s r = getTraceColour $ hitObjects s r

-- Test a ray against each object in the scene. Find the closest object that the ray collided with (if any) and return the obj + shader
hitObjects :: Scene -> Ray -> Traceresult
hitObjects s r
			| length allhits == 0 = Nothing
			| otherwise = 
				case hitresult of
					Just hitpoint -> Just (obj, (shader obj) hitpoint obj s)
					_ -> Nothing
			where
				allhits = hasValue $ [ (obj, ((hit obj) r obj)) | obj <- s]
				(obj, hitresult) = minimumBy (compare `on` snd) allhits 
				hasValue = filter ((/=) Nothing . snd)

-- Iterate over the image and generate camera rays for each pixel. Raytrace each ray to a pixel colour and return the resultant image
render :: Scene -> Integer -> Integer -> Image
render scene width height = (width, height, img)
				where
					img = [ getPixelForRay scene (cameraRay (screenToWorld lx width) (screenToWorld ly height)) |
							ly <- [0..height-1],
							lx <- [0..width-1]]

-- Convert screenspace coordinates to world coordinates
screenToWorld :: Integer -> Integer -> Scalar
screenToWorld screencoord limit = (fromIntegral screencoord) / (fromIntegral $ limit-1) 
