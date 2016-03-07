import Control.Applicative ((<$>), (<*>))
import Data.List (elemIndices, isPrefixOf, sortBy, stripPrefix)
import Data.Maybe
import System.Environment
import System.IO

simplePrefix = "simple_t"
complexPrefix = "complex_t"
smallPrefix = "small_t"
bigPrefix = "big_t"

main = do
    paths <- getArgs
    let f :: FilePath -> (String, Int)
        f x = (getPrefix $ getFileName x, getThreads $ getFileName x)
        sortedPaths = sortBy (\a -> \b -> compare (f a) (f b)) paths
    mapM printStats sortedPaths

average :: [Double] -> Double
average nums = (sum nums) / (fromIntegral $ length nums)

getHeader :: FilePath -> String
getHeader filePath = makeString (getQueryType filename) $ getThreads filename
    where makeString query threads =
              "Query: " ++ query ++ "; Threads: " ++ (show threads)
          filename = getFileName filePath
          getQueryType filename
              | getPrefix filename == simplePrefix = "simple"
              | getPrefix filename == complexPrefix = "complex"
              | getPrefix filename == smallPrefix = "small"
              | getPrefix filename == bigPrefix = "big"

getFileName :: FilePath -> FilePath
getFileName filePath = removeDir filePath
    where removeDir s = let indices = elemIndices '/' s
                            i = if length indices == 0 then -1
                                else last indices
                        in drop (i+1) s

getNums :: Handle -> IO [Double]
getNums handle = do eof <- hIsEOF handle
                    if eof then return []
                    else (:) <$> (read <$> hGetLine handle) <*>  getNums handle

getPrefix :: FilePath -> String
getPrefix fileName = head $ filter (\s -> isPrefixOf s fileName)
                                   [simplePrefix, complexPrefix, smallPrefix
                                   , bigPrefix]

getThreads :: FilePath -> Int
getThreads filename = read $ fromJust $ stripPrefix (getPrefix filename)
                                                    filename

printStats :: FilePath -> IO ()
printStats filePath = do
    putStrLn $ getHeader filePath
    withFile filePath ReadMode $ \handle -> do
        nums <- getNums handle
        putStrLn $ "\taverage: " ++ (show $ average nums)
        putStrLn $ "\tstandard deviation: " ++ (show $ stdDev nums)

stdDev :: [Double] -> Double
stdDev nums = sqrt $! variance nums

variance :: [Double] -> Double
variance nums =
    let avg = average nums
        deviationsSquared = map (**2) $! map (\x -> x - avg) nums
    in average deviationsSquared
