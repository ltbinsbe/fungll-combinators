{-  See sequence A000984 on oeis.org https://oeis.org/A000984 -}

import GLL.Combinators.Interface

instance Parseable Char where
    eos = '$'
    eps = '#'

pX = "X" <::=>  (\xs a -> xs ++ [a]) <$$> pX <**> char 'a'
           <||> (\b xs -> b : xs)    <$$> char 'b' <**> pX
           <||> ((:[])) <$$> char 'z'

test n = length $ parse pX (replicate n 'b' ++ "z" ++ replicate n 'a')
