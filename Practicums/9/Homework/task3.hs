main ::IO()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False

reverseNum :: Int -> Int
reverseNum = read . reverse . show

isPalindrome:: Int -> Bool
isPalindrome x = reverseNum x == x
