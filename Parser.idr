module Parser

import Control.Monad.Identity
import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings
import Stackulator

%access public

pTerm : Parser Term
-- pTerm' : Parser Term

specials : List Char
specials = [' ', '.', ':', '(', ')', '\\']

pVar : Parser Var
pVar =
  do v <- pure (Name . pack) <$> (some $ satisfy (\c => not (c `elem` specials)))
     commitTo (return v)

pTVar : Parser Term
pTVar = pure TVar <$> pVar

someSpace : Parser ()
someSpace = do satisfy isSpace ; skip (many $ satisfy isSpace) <?> "whitespace"

pUniverse : Parser Term
pUniverse =
  do string "Type"
     commitTo $ ((do someSpace
                     i <- integer
                     return (Universe i)
                 ) <|>
                 (do return (Universe 0))
                 )

pPi : Parser Term
pPi =
  string "forall " >!
       do space
          v <- pVar
          space
          colon
          ty <- pTerm
          dot
          space
          e <- pTerm
          return (Pi v ty e)

pLambda : Parser Term
pLambda =
    string "\\" >! do
       space
       v <- pVar
       space
       colon
       ty <- pTerm
       dot
       space
       e <- pTerm
       return (Lambda v ty e)

pApp : Parser Term
pApp =
     do char '('
        t1 <- pTerm
        someSpace
        t2 <- pTerm
        char ')'
        return (App t1 t2)

pTerm = pUniverse <|> pPi <|> pLambda <|> pApp <|> pTVar

instance Show Term where
  show t = ppTerm t

parseAll : Parser a -> String -> Either String a
parseAll f s = let Id r = execParserT f s in case r of
  Success i x =>
    case length i of
     Z => Right x
     _ => Left $ "Incomplete parse: " ++ i
  Failure es  => Left $ formatError s es

test : String -> IO ()
test s =
     case parseAll pTerm s of
       (Left err) => putStrLn err
       (Right t)  => putStrLn $ show t

instance (Show l, Show r)  => Show (Either l r) where
    show (Left l)  = "Left "  ++ show l
    show (Right r) = "Right " ++ show r

-- Local Variables:
-- idris-packages: ("effects" "lightyear")
-- End:
