{-#LANGUAGE OverloadedStrings#-}
import GLL.Combinators.Visit.FUNGLL
import GLL.Parseable.Char()
import GLL.Types.Input
import Data.Text
import GLL.Types.Grammar
import Data.Set as S

choice = (altStart `altOp` alternative1) `altOp` alternative2

nt :: Text
nt = "X"

symb = nterm nt choice -- (altStart `altOp` (parse_apply symb))

alternative1 = (seqStart `seqOp` parse_term 'a') `seqOp` symb

alternative2 = (seqStart `seqOp` parse_term 'b')

pfor = parser_for nt symb (mkInput "b")

firstSymb = getFirstSymb symb

firstSeq1 = getFirstSeq alternative1

firstSeq2 = getFirstSeq alternative2

choice2 :: (Parseable t) => Parse_Choice t
choice2 = (altStart `altOp` seqStart)

firstchoice :: (Parseable t) => Firstset t
firstchoice = getFirstChoice choice2
