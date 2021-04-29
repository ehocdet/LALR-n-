
implementation module daVinci

import StdEnv
import Lalr

todaVinci :: {Kernel} -> String
todaVinci k = daVinci k

daVinci :: {Kernel} -> String
daVinci k = "[" +++ (daVinci` [i \\ i <-: k]) +++ "]"
where daVinci` [] = "\b"
      daVinci` [{name, num, goto, shift, reduce}:lk] =  s +++ (daVinci` lk)
      where 
            s = "l(\"" +++ toString num +++ objectstring +++ ngo +++ sep +++ nsh +++ sep +++ nre +++ "\")],[" +++ ego +++ esh +++ "])),"
            objectstring = "\",n(\"Some Node\",[a(\"OBJECT\",\""

            sep = "--------\\n"
            (_,ngo) = foldl node ((\ {Goto|items}->items),"") goto 
            (_,nsh) = foldl node ((\ {Shift|items}->items),"") shift
            nre = foldl item "" reduce
            node (f,s) e = (f, (foldl item "" i) +++ s)
            where i = f e
            item s {Item| name, rhs} = (addarrow name) +++ "." +++ (foldl (\ x y -> x +++ " " +++ (toString y)) "" rhs) +++ "\\n"+++ s

            (_,ego,i) = foldl edge ((\ {Goto|go, next}->(go,toString next)),"\b",(num+2)*1000) goto
            (_,esh,_) = foldl edge ((\ {Shift|go, next}->(go,toString next)),"\b",i) shift
            edge (f,s,i) e = (f,"l(\"" +++ toString i +++ "\",e(\"\",[a(\"_DIR\",\"none\")],l(\"" +++ toString i +++ objectstring +++ next +++ "\"),a(\"_GO\",\"text\")],[e(\"\",[],r(\"" +++  toString go +++ "\"))]))))," +++ s, i+1)
            where (go,next) = f e

addarrow s = s`%(0,i) +++ "-> " +++ s`%(i+1, size s) +++ " "
where (s`,i) = search 0 s
      search i s | j == size s = (s +++ " ",j)
                 | c == ' '    = (s,i)
                               = search j s
      where j = i+1
            c = s.[i]

