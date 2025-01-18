module Utils where
    data Result r e = Ok r
                    | Err e
                    deriving Show
