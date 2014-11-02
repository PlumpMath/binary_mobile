-- mobile.hs
-- Exercise 2.29 from SICP

import Text.ParserCombinators.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

type Rod = Integer
type Wt  = Integer

data Branch =
     Simple Rod Wt |
     Complex Rod Mobile
     deriving Show

data Mobile =
     Mobile Branch Branch
     deriving Show
     
{- LL(1) CFG for mobile representation
   Mobile --> {Branch Branch}
   Branch --> [integer Mobile]
          --> (integer integer)
-}

lexer       = P.makeTokenParser haskellDef

lexeme      = P.lexeme
whiteSpace  = P.whiteSpace lexer
natural     = P.natural lexer
symbol      = P.symbol lexer

lBracket = symbol "["
rBracket = symbol "]"
lBrace   = symbol "{"
rBrace   = symbol "}"
lParen   = symbol "("
rParen   = symbol ")"

run :: Show a => Parser a -> String -> IO()
run p input =
   case (parse p "" input) of
       Left err -> do
            putStr "parse error at "
            print err
       Right x -> print x

runLex :: Show a => Parser a -> String -> IO()
runLex p input = run (do
    whiteSpace
    x <- p
    eof
    return x)
  input
  
parseMobile :: String -> Either ParseError [Mobile]
parseMobile = parse mobileFile ""

mobileFile :: Parser [Mobile]
mobileFile = do
     whiteSpace
     x <- many mobile
     eof
     return x

mobile :: Parser Mobile
mobile = do
      lBrace
      b1 <- branch
      b2 <- branch
      rBrace
      return (Mobile b1 b2)
  <?> "mobile"

branch :: Parser Branch
branch = do
      lParen
      r <- natural
      w <- natural
      rParen
      return (Simple r w)
  <|> do
      lBracket
      r <- natural
      m <- mobile
      rBracket
      return (Complex r m)
  <?> "branch"

m = parseMobile "     {(8 9)(6 12}\r\n{(6 7)[3{(8 9)(6 12)}]}"



