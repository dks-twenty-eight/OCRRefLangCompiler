module Utils where
    data Result r = Ok r
                  | Err [String]
                      deriving Show

    instance Functor Result where 
        fmap f (Err e) = Err e
        fmap f (Ok r) = Ok (f r)

    instance Applicative Result where
        pure = Ok
        (Ok f) <*> (Ok r) = Ok (f r)
        (Err msgs1) <*> (Err msgs2) = Err (msgs1 ++ msgs2)
        _ <*> (Err e) = Err e
        (Err e) <*> _ = Err e

    instance Monad Result where
        Err e >>= f = Err e
        Ok r  >>= f = f r
        return = Ok
