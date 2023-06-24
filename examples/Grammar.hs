
import GLL.Parser
import GLL.Parseable.Char ()

grammar1 = (start "X" , [prod "X" [nterm "A", nterm "A"]
                      , prod "A" [term 'a']
                      , prod "A" [term 'a', term 'a']
                 ] )

fail1       = "a"
success1    = "aa"
success2    = "aaa"
fail2       = "aaaaa"

