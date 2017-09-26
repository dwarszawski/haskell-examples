sillyExchange :: IO()
sillyExchange = do
  putStrLn "Hello"
  putStrLn "Buddy"
  name <- getLine
  putStrLn $ "Pleased to meet you, " ++ name ++ "!"
