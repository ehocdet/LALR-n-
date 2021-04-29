
implementation module Lexer

import StdEnv
import Token

::LexPos = {line :: Int , column :: Int }

Lex :: [String] -> [(Token,LexPos)]
Lex str = (Layout o Initial) str
where
  Layout :: [(Token,LexPos)] -> [(Token,LexPos)]

  Layout [f=:(_,p=:{column=c}):ts]
	 | 0 == c = [(TokSEPARATOR,p),f : Layout ts]
                  = [f : Layout ts]
  Layout [] = []

  Initial :: [String] -> [(Token,LexPos)]
  Initial s = Lexer {line=1,column=0} s

  CHAR :: !String
  CHAR = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  CHARS :: !String
  CHARS = CHAR +++ "_01233456789"

  LexArray :: {! (!LexPos [String] -> [(Token,LexPos)]) }
  LexArray = {{createArray 256 (LexError "syntax error") & 
     [toInt  l ] = Chars \\ l <-: CHARS} & 
     [toInt '\n'] = NewLine,
     [toInt '\t'] = Tab,
     [toInt ' '] = Space,
     [toInt '/'] = Comments,
     [toInt '='] = Equal,
     [toInt '|'] = Or}

  Lexer :: !LexPos [String] -> [(Token,LexPos)]
  Lexer _ [] = []
  Lexer p=:{column=c} ls=:[s:_] 
	| (size s) > c  = LexArray.[toInt s.[c]] p ls
			= abort ("Lexer abort abnormaly!!!!\n")

  LexError :: !String !LexPos [String] -> [(Token,LexPos)]
  LexError message {line,column} [s:_] = abort (": " +++ toString line +++ ":" +++ message 
          +++ "\n\t" +++ s +++ "\t" +++ createArray column '-' +++ "^\n")

  NewLine :: !LexPos [String] -> [(Token,LexPos)]
  NewLine p [s:xs] = Lexer {line=p.line+1, column=0} xs

  Tab :: !LexPos [String] -> [(Token,LexPos)]
  Tab p s = Lexer {line=p.line, column=p.column+1} s

  Space :: !LexPos [String] -> [(Token,LexPos)]
  Space p s = Lexer {line=p.line, column=p.column+1} s

  Comments :: !LexPos [String] -> [(Token,LexPos)]
  Comments p=:{column=c} ls=:[s:xs]
        | s.[c+1]=='*' = Comment (p,ls) 1 {line=p.line, column=c+2} ls
        | s.[c+1]=='/' = Lexer {line=p.line+1, column=0} xs
                       = LexError "syntax error" p ls

  Comment :: (LexPos,[String]) !Int !LexPos [String] -> [(Token,LexPos)]
  Comment (p,s) 1 _ [] = LexError "unterminated comment" p s
  Comment (p,s) n _ [] = LexError ("unterminated imbricated (" +++ 
		                 toString n +++ " level) comment") p s
  Comment _ 0 p s = Lexer p s
  Comment i n p s = Comment i (n+a) p` s`
  where
    (a,p`,s`) = etat1 p s
    etat1 p=:{column=c} ls=:[{[c]=e}:xs]
        | e=='*'  = etat2 {line=p.line,column=c+1} ls
        | e=='/'  = etat3 {line=p.line,column=c+1} ls
        | e=='\n' = etat1 {line=p.line+1,column=0} xs
	          = etat1 {line=p.line,column=c+1} ls
    etat1 p s = (0,p,s) 

    etat2 p=:{column=c} ls=:[{[c]=e}:xs]
        | e=='*'  = etat2 {line=p.line,column=c+1} ls
        | e=='/'  = (~1,{line=p.line,column=c+1},ls)
        | e=='\n' = etat1 {line=p.line+1,column=0} xs
	          = etat1 {line=p.line,column=c+1} ls
    etat2 p s = (0,p,s) 

    etat3 p=:{column=c} ls=:[{[c]=e}:xs]
        | e=='*'  = (1,{line=p.line,column=c+1},ls)
        | e=='/'  = etat3 {line=p.line,column=c+1} ls
        | e=='\n' = etat1 {line=p.line+1,column=0} xs
	          = etat1 {line=p.line,column=c+1} ls
    etat3 p s = (0,p,s) 


  matchChars :: !String !String !Int -> !Int
  matchChars m s=:{[i]=e} i 
	| or [ t==e \\ t <-: m ] = matchChars m s (i+1)
	    = i-1

  Chars :: !LexPos [String] -> [(Token,LexPos)]
  Chars p=:{column=c} ls=:[s:_]
	  = [ (TokSYMBOL ns,p) : Lexer {line=p.line, column=c+(size ns)} ls ] 
          where ns = s%(c,matchChars CHARS s (c+1)) 

  Equal :: !LexPos [String] -> [(Token,LexPos)]
  Equal p s = [ (TokEQUAL,p) : Lexer {line=p.line, column=p.column+1} s ]

  Or :: !LexPos [String] -> [(Token,LexPos)]
  Or p s = [ (TokOR,p) : Lexer {line=p.line, column=p.column+1} s ]

