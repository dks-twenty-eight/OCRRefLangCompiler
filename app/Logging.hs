module Logging where
    import System.Console.ANSI

    output_error :: String -> IO ()
    output_error err = do
                setSGR [SetColor Foreground Vivid Red] 
                putStr err
                setSGR [Reset]
                putStrLn ""
