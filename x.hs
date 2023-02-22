import Data.List
import Text.Read.Lex (Number)


main =
    do {
        putStrLn "sadge";
        print (show (fib 3));
        print (show (reverse [1,2,3]));
        print (show (myrev2 [1,2,3]));
        print (show (median ["abc","a","bc"]));


    }



fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


myrev2 l  = if null l then [] else last l : myrev2 (init l)

median :: [String] -> Int
median l = 
    if even (length x) then
        ((x !! (xl `div` 2)) + (x !! 1+(xl `div` 2))) `div` 2
    else
        x !! (xl `div` 2)
    where 
        x = sort (map length l)
        xl = length x