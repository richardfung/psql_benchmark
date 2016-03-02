import Control.Applicative ((<$>), (<*>))
import Data.List (elemIndices, isPrefixOf, stripPrefix)
import Data.Maybe
import System.Environment
import System.IO

simplePrefix = "simple_t"
complexPrefix = "complex_t"
smallPrefix = "small_t"
bigPrefix = "big_t"

main = do
    filenames <- getArgs
    print filenames
    putStrLn $ getHeader $ filenames !! 0

average :: [Double] -> Double
average nums = (sum nums) / (fromIntegral $ length nums)

getHeader :: FilePath -> String
getHeader filePath
    | isPrefixOf simplePrefix filename =
        makeString "simple" $ getThreads "simple_t"
    | isPrefixOf complexPrefix filename =
        makeString "complex" $ getThreads "complex_t"
    | isPrefixOf smallPrefix filename =
        makeString "small" $ getThreads "small_t"
    | isPrefixOf bigPrefix filename =
        makeString "big" $ getThreads "big_t"
    where makeString query threads =
              "Query: " ++ query ++ "; Threads: " ++ threads
          getThreads s = fromJust $ stripPrefix s filename
          removeDir s = let indices = elemIndices '/' s
                            i = if length indices == 0 then -1
                                else last indices
                        in drop (i+1) s
          filename = removeDir filePath

getNums :: Handle -> IO [Double]
getNums handle = do eof <- hIsEOF handle
                    if eof then return []
                    else (:) <$> (read <$> hGetLine handle) <*>  getNums handle
                            -- (num:) <$> getNums handle

stdDev :: [Double] -> Double
stdDev nums = sqrt $! variance nums

variance :: [Double] -> Double
variance nums =
    let avg = average nums
        deviationsSquared = map (**2) $! map (\x -> x - avg) nums
    in average deviationsSquared

printStats :: FilePath -> IO ()
printStats filePath = do
    putStrLn $ getHeader filePath
    withFile filePath ReadMode $ \handle -> do
        nums <- getNums handle
        putStrLn $ "\taverage: " ++ (show $ average nums)
        putStrLn $ "\tstandard deviation: " ++ (show $ stdDev nums)
