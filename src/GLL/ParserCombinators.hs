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
help us to define the /derived combinator/s 'within' and 'parens'.

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
module GLL.ParserCombinators (
    module GLL.Combinators.Interface 
    ) where

import GLL.Combinators.Interface
