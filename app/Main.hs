module Main where
   import Lex
   import Utils
   import System.Environment
   import System.IO

   test_preprocessor :: String -> IO ()
   test_preprocessor file = do  print ("Testing file: "++file)
                                handle <- openFile file ReadMode
                                program <-  hGetContents handle
                                let prep = Lex.preprocess program
                                case prep of
                                     Ok p -> putStrLn ("Valid Program: " ++ p)
                                     Err msg -> putStrLn ("Found error: " ++ msg)

   main :: IO ()
   main = do args <- getArgs
             mapM_ test_preprocessor args
