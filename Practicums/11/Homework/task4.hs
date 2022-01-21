main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat = (Int, Int)

simplify :: Rat -> Rat
simplify (x, 0) = error "Can't divide by zero"
simplify (x, y) = ((div x factor), (div y factor))
 where factor = gcd x y

sumRats :: Rat -> Rat -> Rat
sumRats (x1, y1) (x2, y2) = simplify (x1 * y2 + x2 * y1, y1 * y2)

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (x1, y1) (x2, y2) = simplify (x1 * x2, y1 * y2)

divideRats :: Rat -> Rat -> Rat
divideRats (x1, y1) (x2, y2) = simplify (x1 * y2, y1 * x2)

areRatsEqual :: Rat -> Rat -> Bool
areRatsEqual (x1, y1) (x2, y2) = x1 == x2 && y1 == y2

areEqual :: Rat -> Rat -> Bool
areEqual (x1, y1) (x2, y2) = areRatsEqual (simplify (x1, y1)) (simplify (x2, y2))
