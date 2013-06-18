-- Vector stuff
--
module Lib.Vector where

type Scalar = Double
data Vector = Vector [Scalar]

zerovector = Vector [0, 0, 0]
cameraOrigin = Vector [0.5, 0.5, 0]

undef = error "Undefined behaviour"

instance Num Vector where
	(+) (Vector xs) (Vector ys) = Vector (zipWith (+) xs ys)
	(-) (Vector xs) (Vector ys) = Vector (zipWith (-) xs ys)
	(*) (Vector xs) (Vector ys) = Vector (zipWith (*) xs ys)
	negate (Vector xs) = Vector (map negate xs)
	fromInteger x = undef 
	abs (Vector xs) = undef
	signum = undef

instance Eq Vector where
	(==) (Vector xs) (Vector ys) = all id (zipWith (==) xs ys)

instance Show Vector where
    show (Vector xs) = "Vector " ++ show xs

mul :: Scalar -> Vector -> Vector
mul s (Vector xs) = Vector (map (* s) xs)

normalize :: Vector -> Vector
normalize v@(Vector vs) = Vector (map (/mag) vs)
	where mag = (sqrt . vectorsum) (v * v)

vectorsum :: Vector -> Scalar
vectorsum (Vector xs) = sum xs

dot :: Vector -> Vector -> Scalar
dot (Vector xs) (Vector ys) = sum $ zipWith (*) xs ys

mag :: Vector -> Scalar
mag vec = sqrt $ dot vec vec
