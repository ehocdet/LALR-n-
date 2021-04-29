
implementation module Parser

import StdEnv
import Grammar
import Token

bnf :: [(Token,a)] -> [(String,[[String]])]
bnf a = r 
where
   [(_,r):_] = bnf` a

bnf` = <*> rule
where
   rule = (Sep &> Elem) <&> (Equal &> rhs)	// rule = Sep Elem Equal rhs { ($1,$4) }

   rhs =  elems <:&> <*> (Or  &> elems)		// rhs = elems (Or elems)*    { $1:$3 }

   elems = <*> Elem     

   Elem [(TokSYMBOL s,_):xs]    = [(xs,s)]
   Elem  _                      = []
   Sep [x=:(TokSEPARATOR,_):xs] = [(xs,x)]
   Sep    _                     = []
   Equal [x=:(TokEQUAL,_):xs]   = [(xs,x)]
   Equal  _                     = []
   Or    [x=:(TokOR,_):xs]      = [(xs,x)]
   Or     _                     = []
