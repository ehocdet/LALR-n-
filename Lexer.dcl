
definition module Lexer

import StdEnv
import Token

::LexPos = {line :: Int , column :: Int }

Lex  :: [String] ->  [(Token,LexPos)]

