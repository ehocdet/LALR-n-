
implementation module Grammar

import StdEnv

::Parser s r :== [s] -> [([s],r)]

epsilon :: Parser s [r]
epsilon = \xs -> [(xs,[])]

succeed :: r -> Parser s r
succeed v = p 
	where p xs = [(xs,v)]

fail :: Parser s r
fail = \_ -> [] 

(<&>) infixr 6 :: (Parser s a) (Parser s b) -> Parser s (a,b)
(<&>) p1 p2 = p
	where p xs = [ (xs2,(v1,v2)) \\ (xs1,v1) <- p1 xs, (xs2,v2) <- p2 xs1]

(<|>) infixr 4 :: (Parser s a) (Parser s a) -> (Parser s a)
(<|>) p1 p2 = p
	where p xs = p1 xs ++ p2 xs

(<@) infixl 5 :: (Parser s a) (a->b) -> Parser s b
(<@) p0 f  = p
			 where p xs = [(ys, f v) \\ (ys, v) <- p0 xs]

(<&) infixr 6 :: (Parser s a) (Parser s b) -> Parser s a
(<&) p1 p2 = p1 <&> p2 <@ fst

(&>) infixr 6 :: (Parser s a) (Parser s b) -> Parser s b
(&>) p1 p2 = p1 <&> p2 <@ snd

(<:&>) infixr 6 :: (Parser s a) (Parser s [a]) -> (Parser s [a])
(<:&>) p1 p2 = p1 <&> p2 <@ (\(x,xs) -> [x:xs])

<?> :: (Parser s a) -> Parser s [a]
<?> p = p       <@ (\x -> [x])  
    <|> succeed []

<*> :: (Parser s a) -> Parser s [a]
<*> p =     p <:&> <*> p
	<|> succeed []

<+> :: (Parser s a) -> Parser s [a]
<+> p = p <:&> <*> p
