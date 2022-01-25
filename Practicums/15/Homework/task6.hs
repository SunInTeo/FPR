
main :: IO()
main = do
    print (diagonal 5.5) -- --> 5.5
    print (onDiag (5.5,5.5)) -- --> True
    print (onDiag (0.5,0)) -- --> False
    print (onDiag (1,1)) -- True

type Point = (Double, Double)

line :: Point -> Point -> (Double->Double)
line (x1,y1) (x2,y2) = (\x -> y1 + (x-x1)*(y2-y1) / (x2-x1))

diagonal = line (0,0) (1,1)

liesOn :: (Double -> Double) -> Point -> Bool
liesOn f (x, y) = y == f x

onDiag = liesOn diagonal

