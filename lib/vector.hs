-- Vector stuff
--
module Lib.Vector where

type Scalar = Double
data Vector = Vector [Scalar]
-- X refers to the left-right axis of the screen, with X=0 in the top left corner. X=1 in the bottom right.
-- Y refers to the up-down axis of the screen, with Y=0 in the top left corner. Y=1 in the bottom right.
zerovector = Vector [0, 0, 0] -- A vector at the scene origin. This is behind the camera.
cameraOrigin = Vector [0.5, 0.5, 0] -- The camera is in the centre of the screen at z = 0. Rays are passed through the plane Z=1
vecFwd = Vector [0, 0, 1] -- Vector pointing into the screen

instance Num Vector where
	(+) (Vector xs) (Vector ys) = Vector (zipWith (+) xs ys)
	(-) (Vector xs) (Vector ys) = Vector (zipWith (-) xs ys)
	(*) (Vector xs) (Vector ys) = Vector (zipWith (*) xs ys)
	negate (Vector xs) = Vector (map negate xs)
	fromInteger x = error "Vector fromint" -- These operations don't make sense for vectors but are required for Num
	abs (Vector xs) = error "Vector abs"
	signum = error "Vector sign"

instance Ord Vector where -- Vectors can be compared
	(<=) xs ys = mag xs <= mag ys -- By their magnitude

instance Eq Vector where
	(==) (Vector xs) (Vector ys) = all id (zipWith (==) xs ys) -- This is probably not as robust as it could be. FP inaccuracies.

instance Show Vector where
    show (Vector xs) = "Vector " ++ show xs

-- Scalar multiplication of vectors
mul :: Scalar -> Vector -> Vector
mul s (Vector xs) = Vector (map (* s) xs)

-- Return a vector V such that mag V = 1. Preserves the direction of the vector.
normalize :: Vector -> Vector
normalize v@(Vector vs) = Vector (map (/mag) vs)
	where mag = (sqrt . vectorsum) (v * v)

-- Sum the components of a Vector
vectorsum :: Vector -> Scalar
vectorsum (Vector xs) = sum xs

-- Dot product between two vecs
dot :: Vector -> Vector -> Scalar
dot (Vector xs) (Vector ys) = sum $ zipWith (*) xs ys

-- Magnitude (length) of a vector
mag :: Vector -> Scalar
mag vec = sqrt $ dot vec vec
