import Prelude hiding ((<*>), (<*), (<$>), (<$), (*>))

import GLL.Combinators.MemInterface
action = (\x y z -> x+y+z)

tEEE = do 
    tab <- newMemoTable
    let pE = memo tab ("E" <::= 
                                1 <$ char '1'
                            <|> satisfy 0 
                            <|> action <$> pE <*> pE <*> pE 
                      )
--    print (sppf pE (replicate 100 (Char '1')))
--    print (grammar pE)
    res <- parseStringWithOptions[maximumPivot] pE (replicate 2 '1')
    putStrLn (show $ length res)


{-
tAHO = do
    tab <- newMemoTable
    let pS = memo tab ("S" <::=> ((\(x,y) -> x+y+1) <$> char '1' *> pS <*> pS) <|> satisfy 0)
    res <- parseString pS (replicate 200 '1')
    putStrLn (show $ head res)
    return ()
-}

main = tEEE 
