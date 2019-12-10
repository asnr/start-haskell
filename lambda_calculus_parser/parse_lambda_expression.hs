import Control.Applicative
import Control.Monad (liftM, ap)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO()
main = TIO.getContents >>= putStr . show . parseAll . T.filter (not . isWhitespace)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n'

headMay :: T.Text -> Maybe Char
headMay t = if T.null t then Nothing else Just $ T.head t

data ParseSuccess exprT = ParseSuccess { expr :: exprT, unparsedCode :: T.Text }
  deriving (Show)

data Parser exprT = Parser { parseFunc :: T.Text -> Maybe (ParseSuccess exprT) }

instance Monad Parser where
  Parser parseFunction >>= makeNextParser = Parser (\code -> let firstResult = parseFunction code in
    case firstResult of
      Nothing -> Nothing
      Just parseSuccess -> let nextParser = makeNextParser $ expr parseSuccess
                               nextParseFunction = parseFunc nextParser in
        nextParseFunction $ unparsedCode parseSuccess
     )

  return expression = Parser (\code -> Just $ ParseSuccess expression code)

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)

  Parser leftParser <|> Parser rightParser = Parser (\code ->
    leftParser code <|> rightParser code)

-- We are going to ignore lexing for now, so all variable names will be single
-- letters in a-z.
data Expr = Var Char | App Expr Expr | Lam Char Expr deriving (Show)

parseVar :: Parser Expr
parseVar = Parser (\code ->
  do
    c <- headMay code
    if c >= 'a' && c <= 'z'
      then Just $ ParseSuccess (Var c) (T.tail code)
      else Nothing)

parseApp :: Parser Expr
parseApp = do
  consumeToken '('
  leftExpr <- parse
  rightExpr <- parse
  consumeToken ')'
  return (App leftExpr rightExpr)

parseLam :: Parser Expr
parseLam = do
  consumeToken '('
  consumeToken '\\'
  var <- varIdent
  consumeToken '.'
  body <- parse
  consumeToken ')'
  return (Lam var body)

parse :: Parser Expr
parse = parseVar <|> parseApp <|> parseLam

varIdent :: Parser Char
varIdent = Parser (\code ->
  do
    c <- headMay code
    if c >= 'a' && c <= 'z'
      then Just $ ParseSuccess c (T.tail code)
      else Nothing)

consumeToken :: Char -> Parser ()
consumeToken token = Parser (\code ->
  do
    headChar <- headMay code
    if headChar == token
      then Just $ ParseSuccess () (T.tail code)
      else Nothing)

parseAll :: T.Text -> Maybe Expr
parseAll code = let parser = parseFunc parse in
  do
    result <- parser code
    if T.null $ unparsedCode result
      then Just $ expr result
      else Nothing
