import System.Environment

main :: IO()
main = getArgs >>= countLines >>= putStrLn

countLines [fileName] = readFile fileName >>= return . show . length . lines
countLines _ = return "You need help"
