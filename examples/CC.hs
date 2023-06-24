
import GLL.Combinators
import GLL.Parser
import GLL.Parseable.Char ()

fooSPPF = let pX = "A&B" <:=> (++) <$$> pA <**> pB
              pA = "A" <:=> "a" <$$ char 'a' <||> "aa" <$$ char 'a' <** char 'a'
              pB = "B" <:=> "b" <$$ char 'a' <||> "bb" <$$ char 'a' <** char 'a'
              res = parseResultWithOptions [fullSPPF, strictBinarisation] [] pX "aaa"
          in do     putStrLn $ show res
                    putStrLn $ showSPPF $ (sppf_result) res

grammar :: Grammar Token 
grammar = ("X", [Prod "X" [Nt "Y", Term $ Char 'b']
                ,Prod "X" [Term $ Char 'b']
                ,Prod "Y" [Term $ Char 'a']
                ,Prod "Y" [Nt "Z", Term $ Char 'a']
                ,Prod "Z" [Term $ Char 'z']
                ,Prod "Z" [Term $ Epsilon]
                ])
