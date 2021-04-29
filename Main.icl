
module Main

import FormatStream
import Lexer
import Parser
import Lalr
import daVinci

Start world =  (fw,prules)
        where (files,word`) = openfiles world
              (_,fio,files`) = sfopen "gram.sy" 0 files
              (_,fiw,files``) = fopen "gram.daVinci" 1 files`
              fw           = fwrites r fiw

              (kernel,prules) = (Lalr (bnf (Lex (FormatStream fio))))
              r         = todaVinci kernel
