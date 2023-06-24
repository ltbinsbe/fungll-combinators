{-  See sequence https://oeis.org/A001006 -}

import GLL.Combinators.Interface

instance Parseable Char where
    eos = '$'
    eps = '#'

pX = "X" <::=>  (:[])   <$$> char 'a' 
           <||> (:)     <$$> char 'a' <**> pX 
           <||> (\a x1 x2 -> a:x1++x2) <$$> char 'a' <**> pX <**> pX

test n = length $ parse pX ( replicate n 'a' )
