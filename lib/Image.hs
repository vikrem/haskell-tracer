-- PPM output
module Lib.Image where

import Data.Word
import Data.List

type Colour = [Word8]
type Image = (Integer, Integer, [Colour])

create_ppm :: Image -> String
create_ppm (width, height, img) = intercalate "\n" line_list ++ "\n"
    where line_list = ["P3",
						show width ++ " " ++ show height,
						"255",
						concatMap ( (++ "\n") . intercalate " " . map show) img]
