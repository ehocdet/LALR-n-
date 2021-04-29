
definition module Grammar

import StdEnv

::Parser s r :== [s] -> [([s],r)]

epsilon :: Parser s [r]
succeed :: r -> Parser s r
fail :: Parser s r
(<&>) infixr 6 :: (Parser s a) (Parser s b) -> Parser s (a,b)
(<|>) infixr 4 :: (Parser s a) (Parser s a) -> (Parser s a)
(<@) infixl 5 :: (Parser s a) (a->b) -> Parser s b
(<&) infixr 6 :: (Parser s a) (Parser s b) -> Parser s a
(&>) infixr 6 :: (Parser s a) (Parser s b) -> Parser s b
(<:&>) infixr 6 :: (Parser s a) (Parser s [a]) -> (Parser s [a])
<?> :: (Parser s a) -> Parser s [a]
<*> :: (Parser s a) -> Parser s [a]
<+> :: (Parser s a) -> Parser s [a]
