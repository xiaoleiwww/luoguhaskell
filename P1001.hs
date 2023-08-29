main :: IO ()
main = do
    input <- getLine
    -- split string to int list
    let [a, b] = map read $ words input
    print (a+b)