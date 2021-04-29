
definition module Hash
import StdEnv

class tohash a :: !a -> Int

instance tohash {#Char}

::HashTable a b

createHash :: (b->a) Int -> *(HashTable a b) | ==, tohash a
getHash :: (HashTable a b) !a -> [b] | ==, tohash a
putHash :: *(HashTable a b) b -> *(HashTable a b) | tohash a
ugetHash :: *(HashTable a b) !a -> *([b],*(HashTable a b)) | ==, tohash a
delHash :: *(HashTable a b) !a -> *([b],*(HashTable a b)) | ==, tohash a
listHash :: (HashTable a b) -> [b]
