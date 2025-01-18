module Lex where
    import Utils
    import Text.Regex.TDFA
    import Data.Char

    data Token = PASS
               | END
               | IF | THEN | ELSE
               | SWITCH | CASE | DEF 
               | WHILE
               | DO | UNTIL
               | FOR | TO | STEP | NEXT
               | PROC | FUNC | RETURN
               | CLASS | PRIV | PUB | INH
               | OP_ASG
               | OP_PLUS | OP_MINUS | OP_TIMES | OP_DIVI | OP_EXP
               | OP_INTDIV | OP_MOD
               | OP_LT | OP_LEQ | OP_EQ | OP_NEQ | OP_GT | OP_GEQ
               | OP_LAND | OP_LOR | OP_LNOT
               | OP_ACC
               | VAR String 
               | LIT_INT Integer | LIT_CHAR Char | LIT_STR String | LIT_BOOL Bool
               | COMMA
               | LBRA | RBRA | LPAR | RPAR
               | LBRK
               deriving (Eq, Show)
    
    lang_regex :: String
    lang_regex = "pass|end|if|then|else|switch|case|default|while|do|until|for|to|step|next|procedure|function|return|class|private|public|inherits|={1,2}|+|-|\\*|/|^|mod|div|!=|[<>]=?|\\.|[A-Za-z_][A-Za-z_0-9]*|0|[1-9][0-9_]*|0x(0|[1-9A-Za-z][1-9A-Za-z_]*)|0b(0|1[01_]*)|'\\.'|'\\n'|\".*\"|true|false|,|\\(|\\)|\\[|\\]|\n"
    
    {- The below function preprocesses the input program by removing comments and
     - scanning for unterminated string literals, char literals and block comments. 
     - Repeated whitespace is ignored in regex matching, so unimportant to deal with.
     - This is obviously horrible code but necessary to allow case insensitivity for
     - children's benefit...
     - This leaves empty lines in order to be able to provide line numbers in later error messages.
     -}
    
    preprocess :: String -> Result String
    preprocess s = let preproc True       _comment _stringlit _linNr l _pre []                     = Err ["Unterminated block comment beginning in line " ++ (show l)]
                       preproc _blockComm _comment True       _linNr l _pre []                     = Err ["Unterminated string literal in line " ++ show l]
                       preproc _blockComm _comment _stringlit _linNr _ pref []                     = Ok pref
                       preproc False      False    False      lineNr _ pref ('/':'*':f)            = preproc True False False  lineNr lineNr pref f
                       preproc True       _        _          lineNr l pref ('*':'/':f)            = preproc False False False  lineNr 0 (if (l::Integer) == lineNr then pref else pref ++ "\n") f
                       preproc True       _        _          lineNr l pref ('\n':f)               = preproc True  False False  (lineNr+1) l (pref ++ "\n") f
                       preproc True       _        _          lineNr l pref (_:f)                  = preproc True  False  False  lineNr l pref f
                       preproc False      False    False      lineNr l pref ('/':'/':f)            = preproc False True False  lineNr l pref f
                       preproc _          True     _          lineNr l pref ('\n':f)               = preproc False False False  (lineNr+1) l (pref ++ "\n") f
                       preproc _          True     _          lineNr l pref (_:f)                  = preproc False True False  lineNr l pref f
                       preproc False      False    False      lineNr _ pref ('\"':f)               = preproc False False True  lineNr lineNr (pref ++ "\"") f
                       preproc _          _        True       _lineN l _pre ('\n':_)               = Err ["Unterminated string literal in line " ++ show l]
                       preproc _          _        True       lineNr l pref ('\"':f)               = preproc False False False  lineNr l (pref ++ "\"") f
                       preproc _          _        True       lineNr l pref (c:f)                  = preproc False False True   lineNr l (pref ++ [c]) f
                       preproc False      False    False      lineNr _ _    ('*':'/':_)            = Err ["Block comment ending on line " ++ show lineNr ++ " without beginning."]
                       preproc False      False    False      lineNr l pref ('\'':'\\':'n':'\'':f) = preproc False False False  lineNr l (pref ++ "'\\n'") f -- Currently only allowing escaped newline character, not tab or similar.
                       preproc False      False    False      lineNr l pref ('\'':c:'\'':f)        = preproc False False False  lineNr l (pref ++ "'" ++ [c] ++ "'") f
                       preproc False      False    False      lineNr _ _pre ('\'':_:_:_)           = Err ["Invalid character literal in line " ++ show lineNr]
                       preproc False      False    False      lineNr _ _pre ('\'':_:[])            = Err ["Unterminated character literal in line " ++ show lineNr]
                       preproc False      False    False      lineNr l pref ('\n':f)               = preproc False False False (lineNr+1) l (pref ++ "\n") f
                       preproc False      False    False      lineNr l pref (c : f)                = preproc False False False lineNr l (pref ++ [toLower c]) f 
                   in preproc False False False 1 1 "" s
