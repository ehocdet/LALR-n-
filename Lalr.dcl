
definition module Lalr

import StdEnv


Lalr :: [(String,[[String]])] -> ({Kernel},String)

instance toString RuleElems

::Follows
::RuleElems
::Rhs :== [ RuleElems ]
::Item = { name::String, rhs::Rhs, num::Int, firsts::[Follows], follows::[[Follows]] }
::Shift = { go::Int, next::String, items::[Item] }
::Goto = { go::Int, next::String, items::[Item] }
::Kernel = { name::String, num::Int, goto::[Goto], shift::[Shift], reduce::[Item] }


