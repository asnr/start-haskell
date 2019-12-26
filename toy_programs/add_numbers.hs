main :: IO()
main = putStrLn "Type numbers separated by newlines" >> getLine >>= addNumbers 0

addNumbers :: Int -> String -> IO()
addNumbers x "" = putStrLn $ show x
addNumbers x y = getLine >>= addNumbers (x + (read y))
