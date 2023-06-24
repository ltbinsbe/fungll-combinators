
import GLL.ParserCombinators hiding (many, keywords)

import Text.Regex.Applicative

import Control.Monad
import Data.List (inits, isPrefixOf)
import Data.Char

lID :: RawParser Char
lID (t:ts) | isAlpha t = [ t:rest | rest <- inits (takeWhile isIDchar ts) ]
  where isIDchar c = isDigit c || isAlpha c
lID _ = []

lKeys :: RawParser Char
lKeys ts = [ keyw | keyw <- keywords, keyw `isPrefixOf` ts ]

keywords = ["if", "elif", "else"]

bnf_ids :: BNF Char String
bnf_ids = lexical "identifiers" lID

bnf_keys :: BNF Char String
bnf_keys = lexical "keywords" lKeys

bnf_lang :: BNF Char [Phrase]
bnf_lang = multiple bnf_phrase
  where bnf_phrase = "phrase" <::=> ID <$$> bnf_ids <||> KEY <$$> bnf_keys

bnf_lang' :: BNF Char [Phrase]
bnf_lang' = multiple bnf_phrase
  where bnf_phrase = "phrase" <::=> ID <$$> bnf_ids_only <||> KEY <$$> bnf_keys

bnf_lang'' :: BNF Char [Phrase]
bnf_lang'' = multiple bnf_phrase
  where bnf_phrase = "phrase" <::=> ID <$$> bnf_ids_only' <||> KEY <$$> bnf_keys



data Phrase = ID String | KEY String deriving Show

bnf_ids_only :: BNF Char String
bnf_ids_only = lexical "identifiers" lID <\\> bnf_keys

bnf_ids_only' :: BNF Char String
bnf_ids_only' = "ids-no-keywords" <::=> validate <$$$> bnf_ids 
  where validate id = [ id | not (id `elem` keywords) ]

main = do
  putStrLn (unlines (map show (parse bnf_lang "elif")))
  putStrLn (unlines (map show (parse bnf_lang' "elif")))
  putStrLn (unlines (map show (parse bnf_lang'' "elif")))
