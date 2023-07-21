{-#LANGUAGE OverloadedStrings#-}
import GLL.Combinators.Visit.FUNGLL
import GLL.Parseable.Char()
import GLL.Types.Input
import Data.Text
import GLL.Types.Grammar
import Data.Set as S

choice = (altStart `altOp` alternative1) `altOp` seqStart

nt :: Text
nt = "X"

nt2 :: Text
nt2 = "Z"

symb = nterm nt choice -- (altStart `altOp` (parse_apply symb))

alternative1 = (seqStart `seqOp` parse_term 'a') `seqOp` symb

alternative2 = (seqStart `seqOp` parse_term 'b')

pfor = parser_for nt symb (mkInput "")

firstSymb = getFirstSymb symb

firstSeq1 = getFirstSeq alternative1

firstSeq2 = getFirstSeq alternative2



pz = nterm nt pzch

pzch = (altStart `altOp` zseq) `altOp` seqStart

zseq = seqStart `seqOp` parse_term 'z'

px = nterm nt2 pxch

pxch = (altStart `altOp` xseq)

xseq = (seqStart `seqOp` pz) `seqOp` parse_term '1'


firstx = getFirstSeq xseq




pz2 = nterm nt pzch2

pzch2 = (altStart `altOp` zseq2) `altOp` seqStart

zseq2 = seqStart `seqOp` parse_term 'z'

px2 = nterm nt2 pxch2

pxch2 = (altStart `altOp` xseq20) `altOp` xseq21

xseq20 = seqStart `seqOp` pz2

xseq21 = seqStart `seqOp` parse_term '1'
