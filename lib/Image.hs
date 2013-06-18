-- PPM output
module Lib.Image where

import Data.Word
import Data.List

type Colour = [Word8]
type Image = (Integer, Integer, [Colour])

chunkBy :: Int -> [a] -> [[a]]
chunkBy _ [] = []
chunkBy 0 _ = error "Can't chunk by zero!"
chunkBy n xs = [(take n xs)] ++ chunkBy n (drop n xs)

create_ppm :: Image -> String
create_ppm (width, height, img) = intercalate "\n" line_list ++ "\n"
	where
		line_list = ["P3",
						show width ++ " " ++ show height,
						"255",
						chunkline]
		chunks = chunkBy (fromIntegral width) img
		-- omg this is so ugly
		chunkline = concat $ concat $ concatMap ( (++ [["\n"]]) . intersperse ["\t"] . (map (intersperse " " . (map show)) )) chunks 

