import Data.List
import System.Environment

main :: IO()
main = getArgs >>= putStrLn . (intercalate "\n")
