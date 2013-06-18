module Lib.Shader where
import Lib.Vector
import Lib.Image
-- Define shadeable

class Shader a where
	-- Take in a hitpoint, and a normal. Return a color
	shaderFunc :: a -> Vector -> Vector -> Colour
