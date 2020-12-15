import Control.Monad
main = print . solve . map read . lines =<< readFile "in1.in"
solve xs = [ x*y*z | x <- xs, y <- xs, z <- xs, x+y+z==2020 ]
