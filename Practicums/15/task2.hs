main :: IO()
main = do
    print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998
    print $ myPolyHOF [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998

-- myPoly :: [Double] -> (Double -> Int -> Double)
-- myPoly xs = (\ x y -> foldl (\ acc z -> acc * (x - z)) 1 (take y xs))

myPoly :: [Double] -> (Double -> Int -> Double)
myPoly xs = (\ x y -> product [ x - n | n <- (take y xs)])

myPolyHOF :: [Double] -> (Double -> Int -> Double)
myPolyHOF xs = (\ x y -> product $ map (x-) $ take y xs)