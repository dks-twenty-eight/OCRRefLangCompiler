module Main where
   import Lex
   import Utils
   import Logging
   import System.Environment
   import System.IO

   test_preprocessor :: String -> IO ()
   test_preprocessor file = do  print ("Testing file: "++file)
                                handle <- openFile file ReadMode
                                program <-  hGetContents handle
                                let prep = Lex.preprocess program
                                case prep of
                                     Ok p -> putStrLn ("Valid Program: \n" ++ p)
                                     Err msgs -> let msgstr = foldl (\s1 s2 -> "\n" ++ s1 ++ s2) ("Found " ++ (show.length $ msgs) ++ " errors: ") msgs
                                                 in Logging.output_error msgstr
                                putStrLn ""

   main :: IO ()
   main = do args <- getArgs
             mapM_ test_preprocessor args
