
implementation module Hash
import StdEnv

sizeofInt :== 4

class tohash a :: !a -> Int

instance tohash {#Char}
where
   tohash :: !{#Char} -> !Int
   tohash str = hpjw s 0
   where
      s = size str
      hpjw :: !Int !Int -> !Int
      hpjw 0 h = h bitand (0x7fffffff)
      hpjw c h 
        | g<>0 = hpjw (c-1) (h` bitxor (g>>((sizeofInt-1)<<3)) bitxor g)
               = hpjw (c-1) h`

      where  e   = str.[s-c]
             h`  = (h<<4) + (toInt e)
             g   = h` bitand (240 << ((sizeofInt-1)<<3))
    

//:: u:HashTable a b :== u:(b->a,u:{![b]})

::HashTable a b = HT (b->a) .{![b]}

createHash :: (b->a) Int -> *(HashTable a b) | ==, tohash a
createHash g i = HT g (createArray i [])

getHash :: (HashTable a b) !a -> [b] | ==, tohash a
getHash (HT g h) k = [ e \\ e<-l | (g e)==k ]
where
   s = (size h)
   i = (tohash k) mod s
   l = if (i<s && i>=0) h.[i] (abort "out of range")

ugetHash :: *(HashTable a b) !a -> *([b],*(HashTable a b)) | ==, tohash a
ugetHash (HT g h) k = ([ e \\ e<-l | (g e)==k ], (HT g h``))
where
   (s,h`)  = usize h
   i       = (tohash k) mod s
   (l,h``) = if (i<s && i>=0) (uselect h` i) (abort "out of range")

putHash :: *(HashTable a b) b -> *(HashTable a b) | tohash a
putHash (HT g h) e = (HT g {h`` & [i]=[e:l]})
where
    (s,h`) = usize h
    i      = (tohash (g e)) mod s
    (l,h``) = if (i<s && i>=0) (uselect h` i) (abort "out of range")

delHash :: *(HashTable a b) !a -> *([b],*(HashTable a b)) | ==, tohash a
delHash (HT g h) k = (rej, (HT g {h`` & [i]=ok}))
where
     (s,h`) = usize h
     i      = (tohash k) mod s
     (l,h``) = if (i<s && i>=0) (uselect h` i) (abort (foldr (+++) "out of range"
                          [toString (tohash k)," mod ",toString s," = ",toString i]))
     (ok,rej) = select l [] []
     select []    ok rej = (ok,rej)
     select [e:l] ok rej
              | (g e)<>k = select l [e:ok] rej
                         = select l ok [e:rej]

listHash ::  (HashTable a b) -> [b]
listHash (HT _ h) = [ e \\ l<-:h, e<-l]   // [ e \\ l<-:h, e<-l] don't work with Clean 1.3 :-(

