
import GLL.Combinators
import GLL.Parseable.Char()

pX :: BNF Char String
pX = mkRule $
        (    char 'a' **> char 'b' **> pZ
        <||> pY <** char 'b'
        <||> satisfy "" ) 
 where pY = many (char 'y')
       pZ = mkRule $ (:[]) <$$> char 'z'

main = putStrLn $ unlines $ map show $ snd $ grammar pX
