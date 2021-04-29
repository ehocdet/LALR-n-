
implementation module Lalr

import StdEnv
import Hash

::RuleElems = Term String | NonTerm String Int
::Action = Reduce | AShift String | AGoto String
::Follows = Eterm String | Lterm [[Follows]]

::Rhs :== [ RuleElems ]
::Item = { name::String, rhs::Rhs, num::Int, firsts::[Follows], follows::[[Follows]] }
::Shift = { go::Int, next::String, items::[Item] }
::Goto = { go::Int, next::String, items::[Item] }
::Kernel = { name::String, num::Int, goto::[Goto], shift::[Shift], reduce::[Item] }

::SingleRule = { name::String, rhs::[String], num::Int}
::Rule = {name::String, rules::[Item], firsts::Follows}

::SubNode = { go::String, next::Action, items::[Item] } 
::Node = { name::String, num::Int, elems::[SubNode] }


instance toString Follows
where
      toString (Eterm s) = s
      toString (Lterm l) = "(" +++ (foldl (\ x y -> x +++ (foldl (\ x y -> x +++ toString y ) "" y) +++ "|") "" l) +++ ")"

instance toString RuleElems
where
      toString (Term s) = s 
      toString (NonTerm s _) = s 

instance toString Action
where
     toString Reduce = ""
     toString (AShift s) = s
     toString (AGoto s) = s

instance toString Rule
where
     toString {Rule|name,rules,firsts} = "{" +++ toString name +++ " : " +++ toString (extract_firsts 1 firsts) +++ "}\n" +++
            (foldl (\ x y -> x +++ toString y +++ "\n") "" rules)    

instance toString Item
where
     toString {Item|name, rhs} = name +++ " -> " +++ foldl (\ x y -> x +++ " " +++ toString y) "" rhs

startNT = "#S"

nb_follows :== 1

Lalr :: [(String,[[String]])] -> ({Kernel},String)
Lalr r=:[(start,_):_] = (close, (foldl (\ x y -> x +++ (toString y)) "\n" [e \\ e<-:rules]) ) //+++
//                                (foldl (\ x y -> x +++ (toString y) +++ "\n") "\n" [ extract_firsts 1 e \\ e<-:firsts]) )
        where literules = [(startNT,[[start:terms nb_follows]]):r]


              (singlerules,numrules) = foldl op (createHash (\ {SingleRule|name}->name) 47,0) literules
              op (nl,i) (s,ol) = foldl op2 (nl,i) ol
              where op2 (l,ii) e = (putHash l {SingleRule|name=s, rhs=e, num=ii},ii+1)
              
              close = maketable (closure rules)
              rules = formatRules (map fst literules) singlerules

              terms n = term` n []
              where term` 0 l = l
                    term` i l = term` (i-1) ["#" +++ (toString i) : l]


extract_firsts :: Int Follows -> Follows
extract_firsts n f =  snd (ef n f)
where ef n (Eterm s)  = (n-1, Eterm s)
      ef n (Lterm ll) = (maxList nl, Lterm ll`)
      where (nl,ll`)  = unzip (map (ef` n) ll)
            ef` 0 l        = (0,[])      
            ef` n []       = (n,[])
            ef` n [x:l]    = (n``, [l`:ll])
            where (n`, l`) = ef n x
                  (n``,ll) = ef` n` l


formatRules :: [String] (HashTable String SingleRule) -> {Rule}
formatRules ts rules = inrules
where
      (ntrules, numnt) = countnt ts (createHash fst 47) 0
      countnt :: [String] *(HashTable String (String,Int)) Int -> (HashTable String (String,Int),Int) 
      countnt [] nr i = (nr,i)
      countnt [term:ts] nr i = case out of
        [] -> countnt ts (putHash nr` (term, i)) (i+1)
        _  -> countnt ts nr` i
      where (out,nr`) = ugetHash nr term

      inrules = inrules` (listHash ntrules) (createArray numnt "") (createArray numnt []) 

      inrules` :: [(String,Int)] *{String} *{[(SingleRule,Rhs)]} -> {Rule}
      inrules` [] na srh = { {Rule| name=n, rules=r, firsts=f} \\ n<-:na & r<-:ru & f<-:fr }
      where fr = compute_firsts srh
            ru :: {[Item]}
            ru = { map mkrule e \\ e <-: srh }
            mkrule ({SingleRule|name,num},rhs) = {Item| name=name, num=num, rhs=rhs, firsts=map tofirst rhs, follows=[]}
            tofirst (Term s) = Eterm s
            tofirst (NonTerm _ i) = fr.[i]
      inrules` [(nt,i):lnt] na srh = inrules` lnt {na & [i]=nt} {srh & [i]=simple_rules}
      where 
            simple_rules :: [(SingleRule,Rhs)]
            simple_rules = map mkrhs (getHash rules nt)
            mkrhs sr=:{SingleRule| rhs} = (sr,map toItem rhs)
            toItem s = case out of
              [] -> Term s
              [(_,i):_]  -> NonTerm s i
            where out = getHash ntrules s

compute_firsts rules =  firsts
where s = (size rules) - 1

// elimine epsilon a gauche
      no_epsilon_rules :: {(Bool,[Rhs])}
      no_epsilon_rules = elim_epsilon ({isepsilon (map snd e) \\ e <-: rules},s)

      isepsilon el = case [e \\ e=:[] <- el] of
                    [] -> (False,el)
                     _ -> (True,el)
      elim_epsilon (r,m)
         | m <> 0 = elim_epsilon (foldl elim_epsilon` (r,0) [0..m])
                  = r
      where elim_epsilon` (r=:{[i]=be},m) i 
                          | m` > m = (r`,m`)
                                   = (r`,m)
            where (b,e) = be
                  (r`,m`) = esubst e [] r
                  esubst [] lr r                               // plus de regles d'un non term 
                         | b  == True = ({r & [i]=(b,lr)},0)   // regle epsilon deja presante
                         | nb == True = ({r & [i]=blr},i)      // nouvelle regle epsilon -> oblige a reparcourir [0..i]
                                      = ({r & [i]=blr},0)
                         where (blr=:(nb,_)) = isepsilon lr
                  esubst [e=:[(NonTerm _ j):srh] : le] lr r
                         | j <> i && b == True = esubst (new++le) lr r`            // nouvelles regles reinjectees
                                               with new = map (\ x -> x++srh) lj   // substitutions
                                               = esubst le [e:lr] r`
                         where ((b,lj),r`) = uselect r j      // premier non term de la rhs
                  esubst [e:le] lr r = esubst le [e:lr] r

// elimine recursion gauche Mutuelle
      no_rec_rules :: {(Bool,[Rhs])}
      no_rec_rules = elim_rec { (False, e) \\ (_,e) <-: no_epsilon_rules}

      elim_rec r = foldl elim_rec` r [0..s]
      where elim_rec` r=:{[i]=be} i
                      | b == True = r                    // deja calcule
                                  = inrec e [] i [i] r
            where (b,e) = be
                  inrec [] ne i _ r = {r & [i]=(True,ne)}                 // calcule
                  inrec [e=:[(NonTerm _ j):srh] : le] ne i lrg r
                        | i == j || bj    = inrec le [e:ne] i lrg r`      // recGaucheS ou j non rec
                        | isMember j lrg  = inrec le (new++ne) i lrg r`   // recGaucheM sur j -> substitutions sur i
                                          with new = map (\ x -> x++srh) lj
                                          = inrec le [e:ne] i lrg (inrec lj [] j [j:lrg] r`)     // recherche d'une recGaucheM sur j
                        where ((bj,lj),r`) = uselect r j
                  inrec [e:le] ne i lrg r = inrec le [e:ne] i lrg r

// calcul firsts avec elimination des recursions gauche
      firsts :: {Follows}
      firsts = { e \\ e <- r }
      where r :: [Follows]
            r = map (cfirst r) [0..s]
            cfirst r i = Lterm (elimRec rec norec)
            where (rec,norec) = sepRec (snd no_rec_rules.[i])
                  sepRec [e=:[(NonTerm _ j):srh]:l] 
                         | i == j = ([srh:x],y) 
                                  = (x,[e:y])   where (x,y) = sepRec l
                  sepRec [e:l]    = (x,[e:y])   where (x,y) = sepRec l
                  sepRec []       = ([],[]) 

                  elimRec [] norec  = map (map tofirst) norec
                  elimRec rec norec = map tofirst` norec
                  where rec` = Lterm [[] : map tofirst` rec]
                        tofirst` [e:l] = [tofirst e: tofirst` l]
                        tofirst` []    = [rec`]
                  tofirst (Term s) = Eterm s
                  tofirst (NonTerm s i) = r!i



// formatage du noyau LALR
maketable (n,clos) = maketable` (createArray n {Kernel|name="", num=0, goto=[], shift=[], reduce=[]}) (listHash clos) clos
where maketable` t [] _ = t
      maketable` t [{Node| num, name, elems}:sl] c = maketable` {t & [num] = k} sl c 
      where 
            k             = {Kernel| name=name, num=num, goto=got, shift=shi, reduce=red}
            (got,shi,red) = inmaketable ([],[],[]) elems c

      inmaketable (got,shi,red) [] _ = (got,shi,flatten red)
      inmaketable (got,shi,red) [{SubNode| next=Reduce, items} :sl] c = inmaketable (got,shi,[items:red]) sl c	// Reduce
      inmaketable (got,shi,red) [{SubNode| go, next=AShift s, items} :sl] c = 
                                inmaketable (got,[{Shift| go=num, next=s, items=items}:shi],red) sl c
                  where [{Node| num}:_] = getHash c go
      inmaketable (got,shi,red) [{SubNode| go, next=AGoto s, items} :sl] c = 
                                inmaketable ([{Goto| go=num, next=s, items=items}:got],shi,red) sl c
                  where [{Node| num}:_] = getHash c go

// calcul de la fermeture du noyau
closure rules = closure` (createHash (\ {Node|name}->name) 47) [(startNT,items)] 0
where
      {rules=items} = rules.[0]
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
            where inclosure` _    nclos [] = maketrans (listHash nclos) nclos (createHash (\ x -> toString (fst x)) 47)
                  inclosure` clos nclos [{Item|rhs=[]}:ts] = inclosure` clos nclos ts   // pas de fermeture pour une reduction
                  inclosure` clos nclos [{Item|rhs=[Term _:_]}:ts] = inclosure` clos nclos ts
                  inclosure` clos nclos [{Item|rhs=[NonTerm f i:_], firsts=[_:spontane] } : ts] = case out of
                    [] -> inclosure` (foldl putHash clos` t) (foldl putHash nclos t) (t ++ ts) // ajout fermeture du lhs f
                    _  -> inclosure` clos` nclos ts                                     // fermeture du lhs deja calcule
                  where {rules=t} = rules.[i]
                        (out,clos`) = ugetHash clos f

                  next :: Item -> String             // caractere a suivre existe-t-il?
                  next {Item|rhs=[]} = ""                 // non -> ce sera une reduction
                  next {Item|rhs=[s:_]} = toString s      // oui -> un shift

                  maketrans [] _ r = listHash r      // regroupe les regles qui ont le meme first
                  maketrans [t:ts] b r = case out of 
                    [] -> maketrans ts b (putHash r` (h,getHash b nh))
                    _  -> maketrans ts b r`
                  where h = toAction t
                        nh = toString h
                        (out, r`) = ugetHash r nh
                        toAction {Item|rhs=[]} = Reduce
                        toAction {Item|rhs=[Term s:_]} = AShift s
                        toAction {Item|rhs=[NonTerm s i:_]} = AGoto s

            // nomage des noeuds suivant et ajout dans les regles du noeud concerne
            toclose [] l c = (l,c)
            toclose [(Reduce,ts):n] l c = toclose n l [{SubNode| go="", next=Reduce, items=ts}:c]  // cas de la reduction
            toclose [(h,ts):n] l c  = toclose n [(h`,ntrans):l] [{SubNode| go=h`, next=h, items=ts}:c]
            where (heads, ntrans) = addnext ts [] []
                  h` = foldl concat "" heads  // nom du nouveau noeud
                  addnext [] h l = (h,l)      // regles de base du nouveau noeux
                  addnext [{Item| name, rhs=[next:xs], num, firsts=[_:frs], follows} : s] h l = addnext s [head:h] [i:l]    
                  where head = name +++ " " +++ (toString next)
                        i = {Item| name=head, rhs=xs, num=num, firsts=frs, follows=follows}
                  concat "" s = s
                  concat s1 s2 = s1 +++ "|" +++ s2

