-- Math stuff
--

module Lib.Math where
import Lib.Vector

epsilon :: Scalar
epsilon = 0.001

fleq :: Scalar -> Scalar -> Bool
fleq a x = (abs a - abs x) <= epsilon

qdformula :: Scalar -> Scalar -> Scalar -> Maybe [Scalar]
qdformula a b c
	| isNaN det = Nothing
	| fleq det epsilon = Just [b**2 / 2 * a]
	| otherwise = Just [((-b) + det) / 2 * a, ((-b) - det) / 2 * a]
	where det = sqrt $ b**2 - 4 * a * c
