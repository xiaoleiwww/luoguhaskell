import Control.Monad
import Data.Array.IO
import Data.Array.Unboxed
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import System.IO

main :: IO ()
main = do
    [bx, by, mx, my] <- replicateM 4 readInt
    let bx' = bx + 2
        by' = by + 2
        mx' = mx + 2
        my' = my + 2
    f <- newArray (2, by') 0 :: IO (IOUArray Int ll)
    s <- newArray ((2,2), (bx', by')) False :: IO (IOUArray (Int, Int) Bool)
    writeArray f 2 1
    writeArray s (mx', my') True
    forM_ [1..8] $ \i -> do
        let x = mx' + fx !! i
            y = my' + fy !! i
        writeArray s (x, y) True
    forM_ [2..bx'] $ \i -> do
        forM_ [2..by'] $ \j -> do
            sj <- readArray s (i, j)
            if sj then writeArray f j 0
            else do
                fj <- readArray f (j - 1)
                writeArray f j (fj + f j)
    result <- readArray f by'
    print result

readInt :: IO Int
readInt = do
    line <- getLine
    let nums = map read $ words line
    return (head nums)

fx :: [Int]
fx = [0, -2, -1, 1, 2, 2, 1, -1, -2]

fy :: [Int]
fy = [0, 1, 2, 2, 1, -1, -2, -2, -1]
