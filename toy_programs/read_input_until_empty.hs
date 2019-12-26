main :: IO ()
main = getLine >>= getInputUntilEmpty

getInputUntilEmpty :: String -> IO ()
getInputUntilEmpty "" = putStrLn "empty"
getInputUntilEmpty _ = getLine >>= getInputUntilEmpty
