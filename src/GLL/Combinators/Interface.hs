{-# LANGUAGE TypeOperators, FlexibleInstances #-}

{-|

The user writes a combinator expression representing a grammar.
The represented grammar is extracted and given, together with an input string,
to a back-end parser.
The derivations constructed by the parser act as a guide in the "semantic phase"
in which the combinator expressions are evaluated to produce semantic results
for all derivations. Infinitely many derivations would result in a loop.
This problem is avoided by discarding the derivations that would arise from such a loop.

This library provides
"Control.Applicative"-like parser combinators: '<**>' for sequencing, '<||>' for
choice, '<$$>' for application, 'satisfy' instead of pure and derived
combinators '<**', '**>', '<$$', 'many', 'some' and 'optional'.

The semantic phase might benefit from memoisation (see 'memo').
Using memoisation voids pureness waranty".

=== Example usage
This library differs from parser combinator libraries in that combinator expressions
are used to describe a grammar rather than a parser.

A rule is considered to be of the form X ::= a | .. | z, and represented by the combinator
expression.

@
pX = \"X\" '<::=>' altA '<||>' ... '<||>' altZ
@

Alternates (\'a\' ... \'z\') start with the application of
a semantic action using '<$$>' (or variants '<$$' and 'satisfy').
The alternate is extended with '<**>' (or variants '**>', '<**').

@
altA = action1 '<$$>' 'keychar' \'a\' '<**>' pX
altZ = action2 '<$$>' 'keychar' \'z\'
@

Usability is improved by automatic lifting between expressions that represent symbols
and alternates. The main difference with "Control.Applicative" style parser combinator
libraries is that the user must use '<:=>' (or '<::=>') to represent all recursive
nonterminals and must use '<::=>' to represent all nonterminals that potentially lead
to an infinite number of derivations. It is, however, possible to represent left-recursive
nonterminals.

=== Example

In this example we define expressions for parsing (and evaluating) simple arithmetic
expressions for single digit integers.

The library is capable of parsing arbitrary token types that are 'Parseable', orderable
and have a 'Show' instance.
This example demonstrates the usage of the builtin 'Token' datatype and uses the
elementary parsers 'keychar' and 'int_lit' to create parsers for character- and integer-terminals.

We define a very simpler lexer first.

@
lexer :: String -> [Token]
lexer [] = []
lexer (x:xs)
    | isDigit x = 'IntLit' (Just (read [x])) : lexer xs
    | otherwise = 'Char' x                   : lexer xs
@

Note that the Char constructor of the 'Token' type is used for character-level parsing.
Char contains no lexeme, unlike the Int constructor.

Consider the following (highly ambiguous and left-recursive) grammar:

@
Expr ::= Expr \'-\' Expr
       | Expr \'+\' Expr
       | Expr \'*\' Expr
       | Expr \'/\' Expr
       | INT
       | \'(\' Expr \')\'
@

The grammar is translated to the following combinator expression, adding the expected
evaluation functions as semantic actions.

@
pExpr :: BNF Token Int
pExpr = \"Expr\" '<::=>' (-) '<$$>' pExpr '<**' 'keychar' \'-\' '<**>' pExpr
                 '<||>' (+) '<$$>' pExpr '<**' 'keychar' \'+\' '<**>' pExpr
                 '<||>' (*) '<$$>' pExpr '<**' 'keychar' \'*\' '<**>' pExpr
                 '<||>' div '<$$>' pExpr '<**' 'keychar' \'/\' '<**>' pExpr
                 '<||>' 'int_lit'
                 '<||>' parens pExpr
@

Note that '<**' is used to ignore the parse result of the second argument and that '**>'
is used to ignore the parse result of the first argument. These combinators
help us to define the /derived combinator/s /within/ and /parens/.

@
within :: 'BNF' 'Token' a -> 'BNF' 'Token' b -> 'BNF' 'Token' a -> 'BNF' 'Token' b
within l p r = 'mkRule' $ l '**>' p '<**' r

parens :: 'BNF' 'Token' a -> 'BNF' 'Token' a
parens p = within ('keychar' '(') p ('keychar' ')')
@

All possible evaluations are obtained by invoking the 'parse' function.

@
run1 = 'parse' pExpr (lexer "1+2*2-5")            -- [0,1,0,-5,-9]
run2 = 'parse' pExpr (lexer "((1+(2*2))-3)-5")    -- [-3]
@

With every introduction of an operator '+', '-', '*' or '/' the number of ambiguities is
multiplied. The number of ambiguities behaves like the sequence https://oeis.org/A000108.

=== Simple disambiguation

This library offers simple disambiguation strategies that are applied post-parse
(the parser still faces the ambiguity, but the semantic evaluation only yields
the results according to the strategy). The disambiguations strategies are still
in the /experimental/ phase.

We group the operators according to their priorities and use
'<::=' to turn the choice operator '<||>' into a left-biased operator locally
(use 'leftBiased' for the same effect globally).

@
pExpr1 :: BNF Token Int
pExpr1 = \"Expr\" '<::='  (      (-) '<$$>' pExpr1 '<**' 'keychar' \'-\' '<**>' pExpr1
                        '<||>' (+) '<$$>' pExpr1 '<**' 'keychar' \'+\' '<**>' pExpr1 )
                 '<||>' (      (*) '<$$>' pExpr1 '<**' 'keychar' \'*\' '<**>' pExpr1
                        '<||>' div '<$$>' pExpr1 '<**' 'keychar' \'/\' '<**>' pExpr1 )
                 '<||>' (      'int_lit'
                        '<||>' 'parens' pExpr1 )

run3 = 'parseWithOptions' ['maximumPivotAtNt'] pExpr1 (lexer "1+2*2-5") -- [0]
@

The option 'maximumPivotAtNt' enables the 'longest-match' disambiguation strategy
and makes the arithmetic operators left-associative.

=== Grammar rewrites

To deal with the particular ambiguities associated with operators we can
rewrite the grammar to disambiguate pre-parse.

We define the /chainl/ combinator for parsing chains of left-associative operators.

@
chainl :: 'BNF' 'Token' a -> 'BNF' 'Token' (a -> a -> a) -> 'BNF' 'Token' a
chainl p s = 'mkRule' $
    foldl (flip ($)) '<$$>' p '<**>' many (flip '<$$>' s '<**>' p)
@

The expression parser is written with chainl as follows:

@
pExpr2 :: BNF Token Int
pExpr2 = pE1
 where  pE1 = chainl pE2 (\"E1\" '<::=>' (+) '<$$' 'keychar' \'+\' '<||>' (-) '<$$' 'keychar' \'-\')
        pE2 = chainl pE3 (\"E2\" '<::=>' (*) '<$$' 'keychar' \'*\' '<||>' div '<$$' 'keychar' \'/\')
        pE3 = \"E3\" '<::=>' 'int_lit' '<||>' parens pExpr2

run4 = 'parse' 'pExpr2' (lexer "1+2*2-5")       -- [0]
@

Pre-parse disambiguation is desirable, as the parsing process could
speed up dramatically. In general however, it is not always possible to find
the appropriate grammar rewrite and implement it in a high-level combinator such
as chainl, /motivating the existence of this library/.

More simple examples can be found in "GLL.Combinators.Test.Interface".

-}
module GLL.Combinators.Interface (
    -- * Elementary parsers
    term_parser, satisfy,
    -- ** Elementary parsers using the 'Token' datatype
    keychar, keyword, int_lit, float_lit, bool_lit, char_lit, string_lit, alt_id_lit, id_lit, token,
    -- ** Elementary character-level parsers
    char,
    -- * Elementary combinators
    -- *** Sequencing
    (<**>),
    -- *** Choice
    (<||>),
    -- *** Semantic actions
    (<$$>), (<$$$>),
    -- *** Nonterminal introduction
    (<:=>),(<::=>),chooses,chooses_prec,
    -- *** Difference
    -- (<\\>),
    -- * Types
    -- ** Grammar (combinator expression) types
    BNF, SymbExpr, AltExpr, AltExprs,
    -- ** Parseable token types
    Token(..), Parseable(..), SubsumesToken(..), unlexTokens, unlexToken,
    -- * Running a parser
    parse, printParseData, evaluatorWithParseData,
    -- **  Running a parser with options
    parseWithOptions, parseWithParseOptions, evaluatorWithParseDataAndOptions,
    printParseDataWithOptions,
    --printGrammarData,
    -- *** Possible options
    CombinatorOptions, CombinatorOption,
             GLL.Combinators.Options.maximumErrors, throwErrors,
             maximumPivot, maximumPivotAtNt,leftBiased,
    -- *** Running a parser with options and explicit failure
--    parseWithOptionsAndError, parseWithParseOptionsAndError,
    -- ** Runing a parser to obtain 'ParseResult'.
--    parseResult, parseResultWithOptions,ParseResult(..),
    -- ** Builtin lexers.
    default_lexer,
    -- *** Lexer settings
        lexer, lexerEither, LexerSettings(..), emptyLanguage,
    -- * Derived combinators
    mkNt,
    -- *** Ignoring semantic results
    (<$$), (**>), (<**),
    -- *** EBNF patterns
    optional, preferably, reluctantly, optionalWithDef,
    multiple, multiple1, multipleSepBy, multipleSepBy1,
      multipleSepBy2, within, parens, braces, brackets, angles,
      foldr_multiple, foldr_multipleSepBy,
    -- *** Operator expressions
    fromOpTable, opTableFromList, OpTable, Assoc(..), Fixity(..),
     -- *** Disambiguation
            (<:=), (<::=),(<<<**>), (<**>>>), (<<**>), (<<<**), (**>>>), (<**>>),
            longest_match,shortest_match,
            many, many1, some, some1,
            manySepBy, manySepBy1, manySepBy2,
              someSepBy, someSepBy1,someSepBy2,
    -- * Lifting
    HasAlts(..), IsSymbExpr(..), IsAltExpr(..),
     -- * Memoisation
    memo, newMemoTable, memClear, MemoTable, MemoRef, useMemoisation,
     -- * Scannerless parsing, using `RawParser`s
    RawParser, -- lexical,
    ) where

import GLL.Combinators.Options
import GLL.Combinators.Visit.FUNGLL
import GLL.Combinators.Visit.Join
import GLL.Combinators.Visit.Sem
import GLL.Combinators.Memoisation
import GLL.Combinators.Lexer
import GLL.Types.Input
import GLL.Types.Grammar
import GLL.Types.TypeCompose
import GLL.Flags hiding (runOptions)
import GLL.Parseable.Char ()

import Control.Monad (when)
import Control.Arrow
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Text (pack)
import Data.IORef
import Data.Time.Clock
import System.IO.Unsafe

parse' :: (Show t, Parseable t, IsSymbExpr s) => ParseOptions ->
            PCOptions -> s t a -> [t] -> (ParseResult t, Either String [a])
parse' popts opts p' input =
    let SymbExpr (Nt lower_start, vpa2, vpa3) =
          mkRule ("__Augment" <:=> OO [id <$$> p'])
        start       = pack "__Start"
        arr         = mkInput input
        m           = length input
        parse_res   = parser_for start vpa2 arr
        as          = evaluator_for lower_start vpa3 opts (bsrs_result parse_res) arr
        res_list    = unsafePerformIO as
    in (parse_res, if res_success parse_res && not (null res_list)
                    then Right $ res_list
                    else Left (error_message parse_res) )
-- | Print some information about the parse.
-- Helpful for debugging.
printParseData :: (Parseable t, IsSymbExpr s, Show a) => s t a -> [t] -> IO ()
printParseData = printParseDataWithOptions [] []

-- | Variant of 'printParseData' which can be controlled by 'ParseOption's
printParseDataWithOptions :: (Parseable t, IsSymbExpr s, Show a) => ParseOptions -> CombinatorOptions -> s t a -> [t] -> IO ()
printParseDataWithOptions popts opts p' input =
    let SymbExpr (Nt lower_start, vpa2, vpa3) =
          mkRule ("__Augment" <:=> OO [id <$$> p'])
        start       = pack "__Start"
        arr         = mkInput input
        m           = inputLength arr
        parse_res   = parser_for start vpa2 arr
    in do startTime <- getCurrentTime
          putStrLn $ "#tokens:              " ++ (show m)
          putStrLn $ "#successes:           " ++ (show $ res_successes parse_res)
          endTime <- getCurrentTime
          putStrLn $ "recognition time:     " ++ show (diffUTCTime endTime startTime)
          startTime' <- getCurrentTime
          putStrLn $ "#descriptors          " ++ (show $ nr_descriptors parse_res)
          putStrLn $ "#BSRs                 " ++ (show $ nr_bsrs parse_res)
          endTime <- getCurrentTime
          putStrLn $ "parse-data time:      " ++ show (diffUTCTime endTime startTime')
          putStrLn $ "total time:           " ++ show (diffUTCTime endTime startTime)

-- | Print some information
evaluatorWithParseData :: (Parseable t, IsSymbExpr s, Show a) => s t a -> [t] -> [a]
evaluatorWithParseData = evaluatorWithParseDataAndOptions [] []

evaluatorWithParseDataAndOptions :: (Parseable t, IsSymbExpr s, Show a) => ParseOptions -> CombinatorOptions -> s t a -> [t] -> [a]
evaluatorWithParseDataAndOptions popts opts p' input =
    let SymbExpr (Nt lower_start,vpa2,vpa3) = toSymb p'
        start       = pack "__Start"
        parse_res   = parser_for start vpa2 arr
        arr         = mkInput input
        m           = inputLength arr
    in unsafePerformIO $ do
          startTime <- getCurrentTime
          putStrLn $ "#tokens:              " ++ (show m)
          putStrLn $ "#successes:           " ++ (show $ res_successes parse_res)
          endTime <- getCurrentTime
          putStrLn $ "recognition time:     " ++ show (diffUTCTime endTime startTime)
          startTime' <- getCurrentTime
          putStrLn $ "#descriptors          " ++ (show $ nr_descriptors parse_res)
          putStrLn $ "#BSRs                 " ++ (show $ nr_bsrs parse_res)
          endTime <- getCurrentTime
          putStrLn $ "parse-data time:      " ++ show (diffUTCTime endTime startTime')
          startTime' <- getCurrentTime
          as <- evaluator_for start vpa3 (runOptions opts) (bsrs_result parse_res) arr
--          putStrLn $ "#derivations:         " ++ show (length as)
          when (not (null as)) (writeFile "/tmp/derivation" (show (head as)))
          endTime <- getCurrentTime
          putStrLn $ "semantic phase:       " ++ show (diffUTCTime endTime startTime')
          putStrLn $ "total time:           " ++ show (diffUTCTime endTime startTime)
          return as
-- |
-- Runs a parser given a string of 'Parseable's and returns a list of
-- semantic results, corresponding to all finitely many derivations.
parse :: (Show t, Parseable t, IsSymbExpr s) => s t a -> [t] -> [a]
parse = parseWithOptions [throwErrors]

-- |
-- Run the parser with some 'CombinatorOptions'.
parseWithOptions :: (Show t, Parseable t, IsSymbExpr s) =>
                        CombinatorOptions -> s t a -> [t] -> [a]
parseWithOptions opts p ts = parseWithParseOptions defaultPOpts opts p ts

-- |
-- Run the parser with some 'ParseOptions' and 'CombinatorOptions'.
parseWithParseOptions :: (Show t, Parseable t, IsSymbExpr s) =>
                     ParseOptions -> CombinatorOptions -> s t a -> [t] -> [a]
parseWithParseOptions pcopts opts p ts =
    case parseWithParseOptionsAndError pcopts opts p ts of
        Left str | throw_errors opts'   -> error str
                 | otherwise            -> []
        Right as                        -> as
    where opts' = runOptions opts

-- |
-- Run the parser with some 'CombinatorOptions' and return either an error or the results.
-- Any returned results will be a list of length greater than 0.
parseWithOptionsAndError :: (Show t, Parseable t, IsSymbExpr s) =>
                        CombinatorOptions -> s t a -> [t] -> Either String [a]
parseWithOptionsAndError opts p = parseWithParseOptionsAndError defaultPOpts opts p

-- |
-- Run the parser with some 'ParseOptions' and 'CombinatorOptions'.
-- Returns either an error or the results.
-- Any returned results will be a list of length greater than 0.
parseWithParseOptionsAndError :: (Show t, Parseable t, IsSymbExpr s) =>
       ParseOptions -> CombinatorOptions -> s t a -> [t] -> Either String [a]
parseWithParseOptionsAndError popts opts p = (\(_,t) -> t) . parse' defaultPOpts (runOptions opts) p


-- | Get the 'ParseResult', containing an 'SPPF',
--  produced by parsing the given input with the given parser.
parseResult :: (Show t, Parseable t, IsSymbExpr s) => s t a -> [t] -> ParseResult t
parseResult = parseResultWithOptions [] []

-- | Get the 'ParseResult' given some 'ParseOptions' and 'CombinatorOptions'.
parseResultWithOptions :: (Show t, Parseable t, IsSymbExpr s) =>
         ParseOptions -> CombinatorOptions -> s t a -> [t] -> ParseResult t
parseResultWithOptions popts opts p str =
    (\(s,_) -> s) $ parse' popts (runOptions opts) p str

defaultPOpts = [strictBinarisation, packedNodesOnly]

infixl 2 <:=>
-- |
-- Form a rule by giving the name of the left-hand side of the new rule.
-- Use this combinator on recursive non-terminals.
(<:=>) :: (Parseable t, Show t, Ord t, HasAlts b) => String -> b t a -> SymbExpr t a
x <:=> altPs = mkNtRule False False x altPs
infixl 2 <::=>

-- |
--  Variant of '<:=>' for recursive non-terminals that have a potentially infinite
--  number of derivations for some input string.
--
--  A non-terminal yields infinitely many derivations
--  if and only if it is left-recursive and would be
--  left-recursive if all the right-hand sides of the productions of the
--  grammar are reversed.
(<::=>) :: (Parseable t, Show t, Ord t, HasAlts b) => String -> b t a -> SymbExpr t a
x <::=> altPs = mkNtRule True False x altPs

-- | Variant of '<::=>' that can be supplied with a list of alternates
chooses :: (Parseable t, Show t, Ord t, IsAltExpr alt) => String -> [alt t a] -> SymbExpr t a
chooses p alts | null alts = error "chooses cannot be given an empty list of alternatives"
               | otherwise = (<::=>) p (OO (map toAlt alts))

-- | Variant of '<::=' that can be supplied with a list of alternates
chooses_prec :: (Parseable t, Show t, Ord t, IsAltExpr alt) => String -> [alt t a] -> SymbExpr t a
chooses_prec p alts | null alts = error "chooses cannot be given an empty list of alternatives"
                    | otherwise = (<::=) p (OO (map toAlt alts))

infixl 4 <$$>
-- |
-- Form an 'AltExpr' by mapping some semantic action overy the result
-- of the second argument.
(<$$>) :: (Parseable t, Show t, Ord t, IsSymbExpr s) => (a -> b) -> s t a -> AltExpr t b
f <$$> p' = join_apply ((:[]) . f) p'

infixl 4 <$$$>
-- |
-- Variant of `<$$>` that gives access to the underlying ambiguity representation
-- The semantic action can be used to disambiguate, for example using `guard`.
(<$$$>) :: (Parseable t, Show t, Ord t, IsSymbExpr s, Foldable f) => (a -> f b) -> s t a -> AltExpr t b
f <$$$> p' = join_apply f p'


infixl 4 <**>,<<<**>,<**>>>
-- |
-- Add a 'SymbExpr' to the right-hand side represented by an 'AltExpr'
-- creating a new 'AltExpr'.
-- The semantic result of the first argument is applied to the second
-- as a cross-product.
(<**>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) =>
            i t (a -> b) -> s t a -> AltExpr t b
pl' <**> pr' = join_seq [] pl' pr'

-- | Variant of '<**>' that applies longest match on the left operand.
(<**>>>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) =>
            i t (a -> b) -> s t a -> AltExpr t b
pl' <**>>> pr' = join_seq [maximumPivot] pl' pr'

-- | Variant of '<**>' that applies shortest match on the left operand.
(<<<**>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) =>
            i t (a -> b) -> s t a -> AltExpr t b
pl' <<<**> pr' = join_seq [minimumPivot] pl' pr'


-- infixr 5 <\\>
-- (<\\>) :: (Show t) => SymbExpr t a -> SymbExpr t b -> SymbExpr t a
-- p <\\> q = p `join_andNot` q

infixr 3 <||>
-- |
-- Add an 'AltExpr' to a list of 'AltExpr'
-- The resuling  '[] :. AltExpr' forms the right-hand side of a rule.
(<||>) :: (Parseable t, Show t, Ord t, IsAltExpr i, HasAlts b) => i t a -> b t a -> AltExprs t a
l' <||> r' = let l = toAlt l'
                 r = altsOf r'
             in OO (l : r)

-- |
-- Apply this combinator to an alternative to turn all underlying occurrences
-- of '<**>' (or variants) apply 'longest match'.
longest_match :: (Parseable t, Show t, Ord t, IsAltExpr alt) => alt t a -> AltExpr t a
longest_match isalt = AltExpr (v1,v2,\opts -> v3 (maximumPivot opts))
  where AltExpr (v1,v2,v3) = toAlt isalt

-- Apply this combinator to an alternative to turn all underlying occurrences
-- of '<**>' (or variants) apply 'shortest match'.
shortest_match :: (Parseable t, Show t, Ord t, IsAltExpr alt) => alt t a -> AltExpr t a
shortest_match isalt = AltExpr (v1,v2,\opts -> v3 (minimumPivot opts))
  where AltExpr (v1,v2,v3) = toAlt isalt

-- | Create a symbol-parse for a terminal given:
--
--  * The 'Parseable' token represented by the terminal.
--  * A function from that 'Parseable' to a semantic result.
term_parser :: Parseable t => t -> (t -> a) -> SymbExpr t a
term_parser t f = SymbExpr (Term t, parse_term t,\_ _ _ inp l _ -> return [f (fst inp A.! l)])

-- | Create a symbol given a `RawParser` (see `GLL.Types.Input`)
-- lexical :: String -> RawParser t -> SymbExpr t [t]
-- lexical nt regex = join_lexical (pack nt) regex

-- | Parse a single character.
--
-- @
-- char c = term_parser c id
-- @
--
-- Currently, this is the only character-level combinator exported
-- by this module. Please use token-level combinators for practical parsing.
-- Might change in the future.
char :: Char -> SymbExpr Char Char
char c = term_parser c id
{-
-- | Parse a list of characters.
string :: [Char] -> SymbExpr Char [Char]
string [] = mkRule $ satisfy []
string (c:cs) = mkRule $ (:) <$$> char c <**> string cs

-- |
-- Apply a parser within two other parsers.
within :: IsSymbExpr s => BNF Char a -> s Char b -> BNF Char c -> BNF Char b
within l p r = mkRule $ l *> (toSymb p) <* r

-- |
-- Apply a parser within parentheses.
-- parens p = within (char '(') p (char ')')
parens :: BNF Char a -> BNF Char a
parens p = within (char '(') p (char ')')
-}

-- | Parse a single character, using a 'SubsumesToken' type.
keychar :: (Parseable t, SubsumesToken t) => Char -> SymbExpr t Char
keychar c = term_parser (upcast (Char c)) (const c)        -- helper for Char tokens

-- | Parse a single character, using a 'SubsumesToken' type.
keyword :: (Parseable t, SubsumesToken t) => String -> SymbExpr t String
keyword k = term_parser (upcast (Keyword k)) (const k)        -- helper for Char tokens

-- | Parse a single integer, using a 'SubsumesToken' type.
-- Returns the lexeme interpreted as an 'Int'.
int_lit :: (Parseable t, SubsumesToken t) => SymbExpr t Int
int_lit  = term_parser (upcast (IntLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (IntLit (Just i)))  = i
        unwrap _ = error "int_lit: downcast, or token without lexeme"

-- | Parse a single floating point literal, using a 'SubsumesToken' type.
-- Returns the lexeme interpreted as a 'Double'.
float_lit :: (Parseable t, SubsumesToken t) => SymbExpr t Double
float_lit  = term_parser (upcast (FloatLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (FloatLit (Just i)))  = i
        unwrap _ = error "float_lit: downcast, or token without lexeme"

-- | Parse a single Boolean, using a 'SubsumesToken' type.
-- Returns the lexeme interpreter as a Boolean.
bool_lit :: (Parseable t, SubsumesToken t) => SymbExpr t Bool
bool_lit  = term_parser (upcast (BoolLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (BoolLit (Just b)))  = b
        unwrap _ = error "bool_lit: downcast, or token without lexeme"

-- | Parse a single Character literal, using a 'SubsumesToken' type.
-- Returns the lexeme interpreted as a Character literal.
char_lit :: (Parseable t, SubsumesToken t) => SymbExpr t Char
char_lit  = term_parser (upcast (CharLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (CharLit (Just s))) = s
        unwrap _ = error "char_lit: downcast, or token without lexeme"

-- | Parse a single String literal, using a 'SubsumesToken' type.
-- Returns the lexeme interpreted as a String literal.
string_lit :: (Parseable t, SubsumesToken t) => SymbExpr t String
string_lit  = term_parser (upcast (StringLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (StringLit (Just i))) = i
        unwrap _ = error "string_lit: downcast, or token without lexeme"

-- | Parse a single identifier, using a 'SubsumesToken' type.
-- Returns the lexeme as a String.
id_lit :: (Parseable t, SubsumesToken t) => SymbExpr t String
id_lit = term_parser (upcast (IDLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (IDLit (Just i))) = i
        unwrap _ = error "id_lit: downcast, or token without lexeme"

-- | Parse a single alternative identifier, using a 'SubsumesToken' type.
-- Returns the lexeme as a String.
alt_id_lit :: (Parseable t, SubsumesToken t) => SymbExpr t String
alt_id_lit = term_parser (upcast (AltIDLit Nothing)) (unwrap . downcast)
 where  unwrap (Just (AltIDLit (Just i))) = i
        unwrap _ = error "alt_id_lit: downcast, or token without lexeme"


-- | Parse a single arbitrary token, using a 'SubsumesToken' type.
-- Returns the lexeme.
token :: (Parseable t, SubsumesToken t) => String -> SymbExpr t String
token name = term_parser (upcast (Token name Nothing)) (unwrap . downcast)
 where  unwrap (Just (Token name' (Just i))) | name == name' = i
        unwrap _  = error "tokenT: downcast, or token without lexeme"

epsilon :: (Parseable t, Show t, Ord t) => AltExpr t ()
epsilon = AltExpr ([], seqStart ,\_ _ _ _ _ l r ->
                        if l == r then return [(l,())] else return [] )
    where x = "__eps"

-- | The empty right-hand side that yields its
--  first argument as a semantic result.
satisfy :: (Parseable t, Show t, Ord t ) => a -> AltExpr t a
satisfy a = a <$$ epsilon

-- |
-- This function memoises a parser, given:
--
-- * A 'MemoRef' pointing to a fresh 'MemoTable', created using 'newMemoTable'.
-- * The 'SymbExpr' to memoise.
--
-- Use 'memo' on those parsers that are expected to derive the same
-- substring multiple times. If the same combinator expression is used
-- to parse multiple times the 'MemoRef' needs to be cleared using 'memClear'.
--
-- 'memo' relies on 'unsafePerformIO' and is therefore potentially unsafe.
-- The option 'useMemoisation' enables memoisation.
-- It is off by default, even if 'memo' is used in a combinator expression.
memo :: (Parseable t, Ord t, Show t, IsSymbExpr s) => MemoRef [a] -> s t a -> SymbExpr t a
memo ref p' = let   SymbExpr (sym,rules,sem) = toSymb p'
                    lhs_sem opts ctx sppf arr l r
                        | not (do_memo opts) = sem opts ctx sppf arr l r
                        | otherwise = do
                            tab <- readIORef ref
                            case memLookup (l,r) tab of
                                Just as -> return as
                                Nothing -> do   as <- sem opts ctx sppf arr l r
                                                modifyIORef ref (memInsert (l,r) as)
                                                return as
               in SymbExpr (sym, rules, lhs_sem)

-- |
-- Helper function for defining new combinators.
-- Use 'mkNt' to form a new unique non-terminal name based on
-- the symbol of a given 'SymbExpr' and a 'String' that is unique to
-- the newly defined combinator.
mkNt :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> String -> String
mkNt p str = let SymbExpr (myx,_,_) = mkRule p
                in "_(" ++ show myx ++ ")" ++ str

-- | Specialised fmap for altparsers
(.$.) :: (Parseable t, Show t, Ord t, IsAltExpr i) => (a -> b) -> i t a -> AltExpr t b
f .$. i = let AltExpr (s,r,sem) = toAlt i
            in AltExpr (s,r,\opts slot ctx sppf arr l r ->
                                do  as <- sem opts slot ctx sppf arr l r
                                    return $ map (id *** f) as )

-- |
-- Variant of '<$$>' that ignores the semantic result of its second argument.
(<$$) :: (Parseable t, Show t, Ord t, IsSymbExpr s) => b -> s t a -> AltExpr t b
f <$$ p = const f <$$> p
infixl 4 <$$

-- |
infixl 4 **>, <<**>, **>>>

-- |
-- Variant of '<**>' that ignores the semantic result of the first argument.
(**>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) => i t a -> s t b -> AltExpr t b
l **> r = flip const .$. l <**> r

-- Variant of '<**>' that applies longest match on its left operand.
(**>>>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) => i t a -> s t b -> AltExpr t b
l **>>> r = flip const .$. l <**>>> r

-- Variant of '<**>' that ignores shortest match on its left operand.
(<<**>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) => i t a -> s t b -> AltExpr t b
l <<**>r = flip const .$. l <<<**> r


infixl 4 <**, <<<**, <**>>
-- |
-- Variant of '<**>' that ignores the semantic result of the second argument.
(<**) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) => i t a -> s t b -> AltExpr t a
l <** r = const .$. l <**> r

-- | Variant of '<**' that applies longest match on its left operand.
(<**>>) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) => i t a -> s t b -> AltExpr t a
l <**>> r = const .$. l <**>>> r

-- | Variant '<**' that applies shortest match on its left operand
(<<<**) :: (Parseable t, Show t, Ord t, IsAltExpr i, IsSymbExpr s) => i t a -> s t b -> AltExpr t a
l <<<** r = const .$. l <<<**> r

-- |
-- Variant of '<::=>' that prioritises productions from left-to-right (or top-to-bottom).
x <::= altPs = mkNtRule True True x altPs
infixl 2 <::=

-- |
-- Variant of '<:=>' that prioritises productions from left-to-right (or top-to-bottom).
x <:= altPs = mkNtRule False True x altPs
infixl 2 <:=

-- | Try to apply a parser multiple times (0 or more) with shortest match
-- applied to each occurrence of the parser.
many :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t [a]
many = multiple_ (<<<**>)

-- | Try to apply a parser multiple times (1 or more) with shortest match
-- applied to each occurrence of the parser.
many1 :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t [a]
many1 = multiple1_ (<<<**>)

-- | Try to apply a parser multiple times (0 or more) with longest match
-- applied to each occurrence of the parser.
some :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t [a]
some = multiple_ (<**>>>)

-- | Try to apply a parser multiple times (1 or more) with longest match
-- applied to each occurrence of the parser.
some1 :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t [a]
some1 = multiple1_ (<**>>>)

-- | Try to apply a parser multiple times (0 or more). The results are returned in a list.
-- In the case of ambiguity the largest list is returned.
multiple :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t [a]
multiple = multiple_ (<**>)

-- | Try to apply a parser multiple times (1 or more). The results are returned in a list.
-- In the case of ambiguity the largest list is returned.
multiple1 :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t [a]
multiple1 = multiple1_ (<**>)

-- | Internal
multiple_ disa p = let fresh = mkNt p "*"
                    in fresh <::=> ((:) <$$> p) `disa` (multiple_ disa p) <||> satisfy []

-- | Internal
multiple1_ disa p = let fresh = mkNt p "+"
                     in fresh <::=> ((:) <$$> p) `disa` (multiple_ disa p)

-- | Same as 'many' but with an additional separator.
manySepBy :: (Parseable t, Show t, Ord t, IsSymbExpr s, IsSymbExpr s2, IsAltExpr s2) =>
                s t a -> s2 t b -> SymbExpr t [a]
manySepBy = sepBy many
-- | Same as 'many1' but with an additional separator.
manySepBy1 :: (Parseable t, Show t, Ord t, IsSymbExpr s, IsSymbExpr s2, IsAltExpr s2) =>
                s t a -> s2 t b -> SymbExpr t [a]
manySepBy1 = sepBy1 many
-- | Same as 'some1' but with an additional separator.
someSepBy :: (Parseable t, Show t, Ord t, IsSymbExpr s, IsSymbExpr s2, IsAltExpr s2) =>
                s t a -> s2 t b -> SymbExpr t [a]
someSepBy = sepBy some
-- | Same as 'some1' but with an additional separator.
someSepBy1 :: (Parseable t, Show t, Ord t, IsSymbExpr s, IsSymbExpr s2, IsAltExpr s2) =>
                s t a -> s2 t b -> SymbExpr t [a]
someSepBy1 = sepBy1 some
-- | Same as 'multiple' but with an additional separator.
multipleSepBy :: (Parseable t, Show t, Ord t, IsSymbExpr s, IsSymbExpr s2, IsAltExpr s2) =>
                    s t a -> s2 t b -> SymbExpr t [a]
multipleSepBy = sepBy multiple
-- | Same as 'multiple1' but with an additional separator.
multipleSepBy1 :: (Parseable t, Show t, Ord t, IsSymbExpr s, IsSymbExpr s2, IsAltExpr s2) =>
                    s t a -> s2 t b -> SymbExpr t [a]
multipleSepBy1 = sepBy1 multiple

sepBy :: (Parseable t, Show t, Ord t, IsSymbExpr s1, IsSymbExpr s2, IsAltExpr s2) =>
           (AltExpr t a -> SymbExpr t [a]) -> s1 t a -> s2 t b -> SymbExpr t [a]
sepBy mult p c = mkRule $ satisfy [] <||> (:) <$$> p <**> mult (c **> p)

sepBy1 :: (Parseable t, Show t, Ord t, IsSymbExpr s1, IsSymbExpr s2, IsAltExpr s2) =>
           (AltExpr t a -> SymbExpr t [a]) -> s1 t a -> s2 t b -> SymbExpr t [a]
sepBy1 mult p c = mkRule $ (:) <$$> p <**> mult (c **> p)

-- | Like 'multipleSepBy1' but matching at least two occurrences of the
-- first argument. The returned list is therefore always of at least
-- length 2. At least one separator will be consumed.
multipleSepBy2 p s = mkRule $
  (:) <$$> p <** s <**> multipleSepBy1 p s

-- | Like 'multipleSepBy2' but matching the minimum number of
-- occurrences of the first argument as possible (at least 2).
someSepBy2 p s = mkRule $
  (:) <$$> p <** s <**> someSepBy1 p s

-- | Like 'multipleSepBy2' but matching the maximum number of
-- occurrences of the first argument as possible (at least 2).
manySepBy2 p s = mkRule $
  (:) <$$> p <** s <**> manySepBy1 p s

-- | Derive either from the given symbol or the empty string.
optional :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t (Maybe a)
optional p = fresh
  <:=>  Just <$$> p
  <||>  satisfy Nothing
  where fresh = mkNt p "?"

-- | Version of 'optional' that prefers to derive from the given symbol,
-- affects only nullable nonterminal symbols
preferably :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t (Maybe a)
preferably p = fresh
  <:=   Just <$$> p
  <||>  satisfy Nothing
  where fresh = mkNt p "?"

-- | Version of 'optional' that prefers to derive the empty string from
-- the given symbol, affects only nullable nonterminal symbols
reluctantly :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> SymbExpr t (Maybe a)
reluctantly p = fresh
  <:=   satisfy Nothing
  <||>  Just <$$> p
  where fresh = mkNt p "?"

optionalWithDef :: (Parseable t, Show t, Ord t, IsSymbExpr s) => s t a -> a -> SymbExpr t a
optionalWithDef p def = mkNt p "?" <:=> id <$$> p <||> satisfy def

-- | Place a piece of BNF /within/ two other BNF fragments, ignoring their semantics.
within :: (Parseable t, Show t, Ord t, IsSymbExpr s) => BNF t a -> s t b -> BNF t c -> BNF t b
within l p r = mkRule $ l **> toSymb p <** r

-- | Place a piece of BNF between the characters '(' and ')'.
parens p = within (keychar '(') p (keychar ')')
-- | Place a piece of BNF between the characters '{' and '}'.
braces p = within (keychar '{') p (keychar '}')
-- | Place a piece of BNF between the characters '[' and ']'.
brackets p = within (keychar '[') p (keychar ']')
-- | Place a piece of BNF between the characters '<' and '>'.
angles p = within (keychar '<') p (keychar '>')
-- | Place a piece of BNF between two single quotes.
quotes p = within (keychar '\'') p (keychar '\'')
-- | Place a piece of BNF between two double quotes.
dquotes p = within (keychar '"') p (keychar '"')

foldr_multiple :: (IsSymbExpr s, Parseable t) => s t (a -> a) -> a -> BNF t a
foldr_multiple comb def = mkNt comb "-foldr"
  <::=> satisfy def
  <||> ($)      <$$> comb <<<**> foldr_multiple comb def

foldr_multipleSepBy :: (IsSymbExpr s, Parseable t) => s t (a -> a) -> s t b -> a -> BNF t a
foldr_multipleSepBy comb sep def = mkNt comb "-foldr"
  <::=> satisfy def
  <||>  ($ def) <$$> comb
  <||> ($)      <$$> comb <** sep <<<**> foldr_multipleSepBy comb sep def

-- | A table mapping operator keywords to a 'Fixity' and 'Assoc'
-- It provides a convenient way to build an expression grammar (see 'fromOpTable').
type OpTable e  = M.Map Double [(String, Fixity e)]
data Fixity e   = Prefix (String -> e -> e) | Infix (e -> String -> e -> e) Assoc
data Assoc      = LAssoc | RAssoc | NA

opTableFromList :: [(Double, [(String, Fixity e)])] -> OpTable e
opTableFromList = M.fromList

fromOpTable :: (SubsumesToken t, Parseable t, IsSymbExpr s) => String -> OpTable e -> s t e -> BNF t e
fromOpTable nt ops rec = chooses_prec (nt ++ "-infix-prefix-exprs") $
  [ mkNterm ix row
  | (ix, row) <- zip [1..] (M.elems ops)
  ]
  where mkNterm ix ops = chooses (ntName ix) $
          [ mkAlt op fix | (op, fix) <- ops ]
          where mkAlt op fix = case fix of
                  Prefix f -> f <$$> keyword op <**> rec
                  Infix f assoc -> case assoc of
                    LAssoc -> f <$$> rec <**> keyword op <**>>> rec
                    RAssoc -> f <$$> rec <**> keyword op <<<**> rec
                    _      -> f <$$> rec <**> keyword op <**> rec

        ntName i = show i ++ nt ++ "-op-row"

