
module Main where

import Control.Monad
import qualified GLL.Combinators.MemInterface as Mem
import GLL.Combinators.Interface

-- | Datatype representing arithmetic expressions
data Expr   = Add Expr Expr
            | Min Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            | Par Expr
            | Lit Int
    deriving (Ord, Eq)

-- | Evaluate an 'Expr'
eExpr :: Expr -> Int
eExpr expr' = 
    case expr' of
    Par expr        -> eExpr expr
    Lit i           -> i
    Mul e1 e2       -> eExpr e1 * eExpr e2
    Div e1 e2       -> let e2_res = eExpr e2
                        in if e2_res <= 0 
                            then 0
                            else eExpr e1 `div` e2_res
    Add e1 e2       -> eExpr e1 + eExpr e2            
    Min e1 e2       -> eExpr e1 - eExpr e2           

-- | A parser/concrete syntax for the abstract syntax 'Expr'
ioExpr :: IO (Mem.Parser Expr)
    = do tab <- Mem.newMemoTable
         let  pExpr = "Expr" Mem.<::= (Mem.memo tab $ 
                            (    Min Mem.<$> pExpr Mem.<* Mem.char '-' Mem.<*> pExpr
                            Mem.<|> Add Mem.<$> pExpr Mem.<* Mem.char '+' Mem.<*> pExpr)
                        Mem.<|> (   Mul Mem.<$> pExpr Mem.<* Mem.char '*' Mem.<*> pExpr
                            Mem.<|> Div Mem.<$> pExpr Mem.<* Mem.char '/' Mem.<*> pExpr)
                        Mem.<|> (   Lit Mem.<$> pDigit
                            Mem.<|> Par Mem.<$ Mem.char '(' Mem.*> pExpr Mem.<* Mem.char ')')
                        )
              pDigit =  "Digit" Mem.<::=> 0 Mem.<$ Mem.char '0'
                            Mem.<|> 1 Mem.<$ Mem.char '1'
                            Mem.<|> 2 Mem.<$ Mem.char '2'
                            Mem.<|> 3 Mem.<$ Mem.char '3'
                            Mem.<|> 4 Mem.<$ Mem.char '4'
                            Mem.<|> 5 Mem.<$ Mem.char '5'
                            Mem.<|> 6 Mem.<$ Mem.char '6'
                            Mem.<|> 7 Mem.<$ Mem.char '7'
                            Mem.<|> 8 Mem.<$ Mem.char '8'
                            Mem.<|> 9 Mem.<$ Mem.char '9'

         return pExpr 

pExpr = "Expr" <::= (
                    (   Min <$> pExpr <* char '-' <*> pExpr
                    <|> Add <$> pExpr <* char '+' <*> pExpr)
                <|> (   Mul <$> pExpr <* char '*' <*> pExpr
                    <|> Div <$> pExpr <* char '/' <*> pExpr)
                <|> (   Lit <$> pDigit
                    <|> Par <$> parens pExpr)
                )

-- | Parse a digit
pDigit :: Parser Int
pDigit = "Digit" <::=> 0 <$ char '0'
            <|> 1 <$ char '1'
            <|> 2 <$ char '2'
            <|> 3 <$ char '3'
            <|> 4 <$ char '4'
            <|> 5 <$ char '5'
            <|> 6 <$ char '6'
            <|> 7 <$ char '7'
            <|> 8 <$ char '8'
            <|> 9 <$ char '9'

-- | Parser variant which uses chainl (grammar rewrite)
pExpr' :: Parser Expr
pExpr' = pE1
 where  pE1 = chainl pE2 ("E1" <:=> Add <$ char '+' <|> Min <$ char '-')
        pE2 = chainl pE3 ("E2" <:=> Mul <$ char '*' <|> Div <$ char '/')
        pE3 = "E3" <:=> Lit <$> pDigit <|> Par <$> parens pExpr'

-- | Pretty-print a string
ppExpr :: Expr -> String
ppExpr e = 
    case e of 
    Par e       -> "(" ++ ppExpr e ++ ")"
    Lit i       -> show i
    Mul e1 e2   -> "(" ++ ppExpr e1 ++ "*" ++ ppExpr e2 ++ ")"
    Div e1 e2   -> "(" ++ ppExpr e1 ++ "/" ++ ppExpr e2 ++ ")"
    Add e1 e2   -> "(" ++ ppExpr e1 ++ "+" ++ ppExpr e2 ++ ")"
    Min e1 e2   -> "(" ++ ppExpr e1 ++ "-" ++ ppExpr e2 ++ ")"

-- | Pretty-print an expression with its eval result
ppResult :: Expr -> String
ppResult e = ppExpr e ++ " = " ++ (show $ eExpr e)

-- | Run the parser on a test-string, with disambiguation options
--run_disambig :: Options -> Parser Expr -> String -> [String]
--run_disambig opts p str = map ppResult (parseStringWithOptions opts p str)
{-
run_disambig :: Options -> Parser Expr -> String -> IO Int 
run_disambig opts p str = do let exprs = parseStringWithOptions opts p str
                             return $ length exprs


mem_run_disambig :: Mem.Options -> IO (Mem.Parser Expr) -> String -> IO Int 
mem_run_disambig opts iop str = do  p <- iop
                                    exprs <- Mem.parseStringWithOptions opts p str
                                    return $ length exprs
-}

main :: IO ()
main = return ()

-- | Should produce: 3
string_4 = "5-2-1*0"

-- | Should produce: 3
string_5 = "5+2-1*0"

-- | Should produce: 7
string_7 = "5-(2-1)*0+7*6/(7*3)"

-- | Should produce: 5
string_8 = "5-(2-1)*0+7*6/(7*3)*0"

-- | Create a 'String' of the given length
string_x :: Int -> String
string_x x' = 
 let    x   | even x'   = x' - 1
            | otherwise = x'
        digits = drop 42 $ cycle ['0'..'9']
        ops    = drop 8  $ cycle ['*', '-', '/', '+']
 in take x $ tail $ concat $ zipWith (\x y -> [x,y]) ops digits

-- == Helpers

-- | A parser within parenthesis
parens :: Parser a ->  Parser a
parens p = mkParser $ char '(' *> p <* char ')'

chainl :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl p s = mkParser $
    foldl (flip ($)) <$> p <*> many (flip <$> s <*> p)
