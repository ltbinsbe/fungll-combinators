
import GLL.Combinators.Interface

import Data.Char (isDigit)

lexer :: String -> [Token]
lexer [] = []
lexer (x:xs) 
    | isDigit x = Int (Just (read [x])) : lexer xs
    | otherwise = Char x                : lexer xs


pExpr :: Parser Token Int
pExpr = "Expr" <::=> (-) <$$> pExpr <** charT '-' <**> pExpr
                <||> (+) <$$> pExpr <** charT '+' <**> pExpr
                <||> (*) <$$> pExpr <** charT '*' <**> pExpr
                <||> div <$$> pExpr <** charT '/' <**> pExpr
                <||> intT 
                <||> parens pExpr

within :: Parser Token a -> Parser Token b -> Parser Token a -> Parser Token b
within l p r = mkParser $ l **> p <** r

parens :: Parser Token a -> Parser Token a
parens p = within (charT '(') p (charT ')')

run1 = parse pExpr (lexer "1+2*2-5")            -- [0,1,0,-5,-9] 
run2 = parse pExpr (lexer "((1+(2*2))-3)-5")    -- [-3]

pExpr1 :: Parser Token Int
pExpr1 = "Expr" <::=  (     (-) <$$> pExpr1 <** charT '-' <**> pExpr1
                        <||> (+) <$$> pExpr1 <** charT '+' <**> pExpr1 )
                 <||> (      (*) <$$> pExpr1 <** charT '*' <**> pExpr1
                        <||> div <$$> pExpr1 <** charT '/' <**> pExpr1 )
                 <||> (      intT 
                        <||> parens pExpr1 )

run3 = parseWithOptions [maximumPivotAtNt] pExpr1 (lexer "1+2*2-5")

chainl :: Parser Token a -> Parser Token (a -> a -> a) -> Parser Token a
chainl p s = mkParser $
    foldl (flip ($)) <$$> p <**> many (flip <$$> s <**> p)

pExpr2 :: Parser Token Int
pExpr2 = pE1
 where  pE1 = chainl pE2 ("E1" <::=> (+) <$$ charT '+' <||> (-) <$$ charT '-')
        pE2 = chainl pE3 ("E2" <::=> (*) <$$ charT '*' <||> div <$$ charT '/')
        pE3 = "E3" <::=> intT <||> parens pExpr2

run4 = parse pExpr2 (lexer "1+2*2-5")       -- [0]
