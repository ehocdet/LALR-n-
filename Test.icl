
module Test

import StdEnv
import Hash

instance toString RuleElems
where
      toString (Term s) = s 
      toString (NonTerm s) = s 

::RuleElems = Term String | NonTerm String

::Rhs :== [ RuleElems ]
::Item = { name::String, rhs::Rhs, num::Int, firsts::[String], follows::Int }
::Shift = { go::Int, next::RuleElems, items::[Item] }
::Goto = { go::Int, next::RuleElems, items::[Item] }
::Kernel = { name::String, num::Int, goto::[Goto], shift::[Shift], reduce::[Item] }

::SingleRule = { name::String, rhs::[String], num::Int}
::Rule = {name::String, firsts::([] String), rules::[Item], num::Int}

::SubNode = { go::String, next::RuleElems, items::[Item] } 
::Node = { name::String, num::Int, elems::[SubNode] }


nb_follows :== 0

Lalr :: [(String,[[String]])] -> {Kernel}
Lalr r=:[(start,_):_] = close
        where literules = [("#START",[[start:terms nb_follows]]):r]

              (singlerules,numrules) = foldl op (createHash (\ {SingleRule|name}->name) 47,0) literules
              op (nl,i) (s,ol) = foldl op2 (nl,i) ol
              where op2 (l,ii) e = (putHash l {SingleRule|name=s, rhs=e, num=ii},ii+1)
              
              close  = maketable rules (closure rules "#START")
              rules = inrules (map fst literules) singlerules

              terms n = term` n []
              where term` 0 l = l
                    term` i l = term` (i-1) ["#" +++ (toString i) : l]

//inrules :: [String] (HashTable String SingleRule) -> (HashTable String Rule)
inrules ts rules = inrules` ts 0 (createHash (\ {Rule|name}->name) 47)
where
      inrules` [] _ nr = nr
      inrules` [term:ts] i nr = case out of
        [] -> inrules` ts (i+1) (putHash nr` {Rule| name=term, num=i, rules=simple_rules, firsts=[]})
        _  -> inrules` ts i nr` 
      where (out,nr`) = ugetHash nr term
            simple_rules = foldl mkitem [] (getHash rules term)
            mkitem ni {SingleRule| name, rhs, num} = [{Item| name=name, rhs=(map toItem rhs), num=num, firsts=[], follows= -1}:ni]
            toItem s = case out of
              [] -> Term s
              _  -> NonTerm s
            where out = getHash rules s


// liste des nonterms uniq avec num et rules associes

//firsts :: [String] (HashTable String SingleRule) -> (HashTable String Rule)

firsts rules n = firsts` rules (createArray n {Rule| name="!@#$", num= -1, rules=[], firsts=[]})
where
      firsts` [] ar = ar
      firsts` [{name,num,rules}:rs] ar = firsts` rs {ar & [num]={name=name, num=num, rules=rules, firsts=fsts}}
      where fsts = []



allfollows [] = []
allfollows list = [(flatten firsts) : allfollows reste]
where
      (firsts,reste)  = unzip [(f,fs) \\ [f:fs] <- list]

// formatage du noyau LALR
maketable rules (n,clos) = maketable` (createArray n {Kernel|name="", num=0, goto=[], shift=[], reduce=[]}) (listHash clos) clos
where maketable` t [] _ = t
      maketable` t [{Node| num, name, elems}:sl] c = maketable` {t & [num] = k} sl c 
      where 
            k             = {Kernel| name=name, num=num, goto=got, shift=shi, reduce=red}
            (got,shi,red) = inmaketable ([],[],[]) elems c

      inmaketable (got,shi,red) [] _ = (got,shi,flatten red)
      inmaketable (got,shi,red) [{SubNode| next=(Term ""), items} :sl] c = inmaketable (got,shi,[items:red]) sl c	// Reduce
      inmaketable (got,shi,red) [{SubNode| go, next=(next=:(Term s)), items} :sl] c = 
                                inmaketable (got,[{Shift| go=num, next=next, items=items}:shi],red) sl c
      where [{Node| num}:_] = getHash c go
      inmaketable (got,shi,red) [{SubNode| go, next=(next=:(NonTerm s)), items} :sl] c = 
                                inmaketable ([{Goto| go=num, next=next, items=items}:got],shi,red) sl c
      where [{Node| num}:_] = getHash c go
      /*
      inmaketable (got,shi,red) [{SubNode| go, next, items} :sl] c = case out of
          [] -> inmaketable (got,[{Shift| go=num, next=next, items=items}:shi],red) sl c	// Shift (Terminal)
          _  -> inmaketable ([{Goto| go=num, next=next, items=items}:got],shi,red) sl c         // Goto  (Non terminal)
      where [{Node| num}:_] = getHash c go
            out             = getHash rules next
*/

// calcul de la fermeture du noyau
closure rules start = closure` (createHash (\ {Node|name}->name) 47) [(start,items)] 0
where
      [{rules=items}:_] = getHash rules start
      closure` clos [] num = (num,clos)
      closure` clos [(name,l):o] num = case out of
        [] -> let! (tclose,nclose) = toclose (inclosure l) [] []                     // fermeture du noeud
              in closure` (putHash clos` {Node| num=num, name=name, elems=nclose}) (o ++ tclose) (num+1)
        _  -> closure` clos` o num                                                   // fermeture du noeud deja calcule
      where (out,clos`) = ugetHash clos name

            // fermeture des regles d'un etat du noyau
            inclosure l = inclosure` (foldl putHash (createHash (\ {Item| name}->name) 47) l)   // hash en fonction du lhs
                                     (foldl putHash (createHash next 47) l)             // hash en fonction du first
                                     l
            where inclosure` _    nclos [] = maketrans (listHash nclos) nclos (createHash fst 47)
                  inclosure` clos nclos [{Item|rhs=[]}:ts] = inclosure` clos nclos ts   // pas de fermeture pour une reduction
                  inclosure` clos nclos [{Item|rhs=[Term f:_]}:ts] = inclosure` clos nclos ts
                  inclosure` clos nclos [{Item|rhs=[NonTerm f:_]}:ts] = case out of
                    [] -> inclosure` (foldl putHash clos` t) (foldl putHash nclos t) (t ++ ts) // ajout fermeture du lhs f
                    _  -> inclosure` clos` nclos ts                                     // fermeture du lhs deja calcule
                  where [{rules=t}:_] = getHash rules f
                        (out,clos`) = ugetHash clos f

                  next :: Item -> String             // caractere a suivre existe-t-il?
                  next {Item|rhs=[]} = ""                 // non -> ce sera une reduction
                  next {Item|rhs=[s:_]} = toString s      // oui -> un shift

                  maketrans [] _ r = listHash r      // regroupe les regles qui ont le meme first
                  maketrans [t:ts] b r = case out of 
                    [] -> maketrans ts b (putHash r` (h,getHash b h))
                    _  -> maketrans ts b r`
                  where h = next t
                        (out, r`) = ugetHash r h

            // nomage des noeuds suivant et ajout dans les regles du noeud concerne
            toclose [] l c = (l,c)
            toclose [("",ts):n] l c = toclose n l [{SubNode| go="", next=(Term ""), items=ts}:c] // cas partuculier de la reduction
            toclose [(h,ts):n] l c  = toclose n [(h`,ntrans):l] [{SubNode| go=h`, next=NonTerm h, items=ts}:c]
            where (heads, ntrans) = addnext ts [] []
                  h` = foldl concat "" heads  // nom du nouveau noeud
                  addnext [] h l = (h,l)      // regles de base du nouveau noeux
                  addnext [{Item| name, rhs=[next:xs], num}:s] h l = addnext s [head:h] [i:l]    
                  where head = name +++ " " +++ (toString next)
                        i =  {Item| name=head, rhs=xs, num=num, firsts=[], follows= -1}
                  concat "" s = s
                  concat s1 s2 = s1 +++ "|" +++ s2

