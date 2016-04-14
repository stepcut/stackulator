module Parser
{-
Syntax is hard, let's go shopping!

This version of the syntax is easy to parse -- no backtracking required?

let VAR = TERM
VAR
LIT
\VAR[:TERM]. TERM
(TERM TERM)

-}
{-

let VAR = TERM
-- pTerm
VAR
LIT
\VAR0 [.. VAR_N] => TERM
TERM0 [.. TERM_N]
(TERM)


Syntax examples:

let id    = \ a x => x : forall (a : *) . a -> a
let const = (\ a b x y => x) : forall (a : *) (b : *) . a -> b -> a

3
+

3 : Double

How are parens handled?

-}

import Control.Monad.Identity
import Data.List
import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings
import Stackulator

%access public

||| parse a `Term`
pTerm : Parser Term

specials : List Char
specials = [' ', '.', ':', '(', ')', '\\']

reserved : List String
reserved = ["let", "forall", "=>", "Type"]

||| parse a `Var`
pVar : Parser Var
pVar =
  do v <- pure pack <$> (some $ satisfy (\c => not (c `elem` specials)))
     if (v `elem` reserved)
      then fail $ v ++ " is a reserved keyword"
      else commitTo (return (Name v))

||| parse a `Var` and lift it to a `Term`
pTVar : Parser Term
pTVar = pure TVar <$> pVar

-- ||| skip 
-- manySpace : Parser ()
-- manySpace = do satisfy isSpace ; skip (many $ satisfy isSpace) <?> "whitespace"

-- FIXME: handle - and . correctly

||| parse a `Double`
pDouble : Parser Literal
pDouble =
  do str <- some $ satisfy (\c => c `elem` (unpack $ "1234567890.-"))
     pure (LDouble $ cast $ pack str)


||| parse any `Literal`
pLiteral : Parser Literal
pLiteral = pDouble


||| parse a `Literal` and lift it to a `Term`
pLit : Parser Term
pLit = pure Lit <$> pLiteral

||| parse `Primitive`
pPrimitive : Parser Primitive
pPrimitive =
        (char '+' $> pure Plus)  <|>
        (char '-' $> pure Minus) <|>
        (char '*' $> pure Times) <|>
        (char '/' $> pure Divide)

||| parse `Primitive` and lift to `Term`
pPrim : Parser Term
pPrim = pure Prim <$> pPrimitive


||| parse a `Universe` `Term`
pUniverse : Parser Term
pUniverse =
  do string "Type"
     commitTo $ ((do space
                     i <- integer
                     return (Universe i)
                 ) <|>
                 (do return (Universe 0))
                 )


||| parse a `Pi` `Term`
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

||| parse a `Lambda` `Term`
pLambda : Parser Term
pLambda =
  do string "\\"
     vtys <- commitTo $ some $ do
                           space
                           v <- pVar <$ space
                           colon
                           ty <- commitTo $ pTerm
                           return (v, ty)
     space
--     string "=>"
     commitTo $ char '.'
     space
     e <- commitTo pTerm
     return (foldr (\(v,ty) => \e' =>  Lambda v ty e') e vtys)

||| parse application (term0 .. termN)
pApp : Parser Term
pApp =
     do char '('
        (t::ts) <- sepBy1 pTerm space
        char ')'
        return $ foldl App t ts


pTerm = pPrim <|> pUniverse <|> pPi <|> pLit <|> pTVar <|> pLambda <|> pApp

instance Show Term where
  show t = ppTerm t

pLet : Parser Statement
pLet =
     string "let " >!
       do v <- pVar
          space
          char '='
          space
          t <- pTerm
          commitTo $ return $ Let v t

pAssume : Parser Statement
pAssume =
     string "assume " >!
       do v <- pVar
          space
          char ':'
          space
          t <- pTerm
          commitTo $ return $ Assume v t

pForget : Parser Statement
pForget =
  string "forget " >!
    do v <- pVar
       commitTo $ return $ Forget v

pStatement : Parser Statement
pStatement =
 pLet <|> pAssume <|> pForget <|> (pure STerm <$> pTerm)

||| run a `Parser` ensuring that the entire `String` is consumed
parseAll : Parser a -> String -> Either String a
parseAll f s = let Id r = execParserT f s in case r of
  Success i x =>
    case length i of
     Z => Right x
     _ => Left $ "Incomplete parse: " ++ i
  Failure es  => Left $ formatError s es

prs : Parser Term -> String -> String
prs p str = case parseAll p str of
          (Right e)  => ppTerm e
          (Left err) => err

||| run a `Parser` on a `String` and show the results
testp : (Show a) => Parser a -> String -> IO ()
testp p s =
     case parseAll p s of
       (Left err) => putStrLn err
       (Right t)  => putStrLn $ show t

||| `testp pTerm s`
test : String -> IO ()
test s = testp pTerm s

instance (Show l, Show r)  => Show (Either l r) where
    show (Left l)  = "Left "  ++ show l
    show (Right r) = "Right " ++ show r

-- Local Variables:
-- idris-packages: ("effects" "lightyear")
-- End:
