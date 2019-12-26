import System.Environment

main :: IO()
main = getArgs >>= cat >>= putStrLn

cat [fileName] = readFile fileName
cat _ = return "You need help"
