import Control.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO()
main = TIO.getContents >>= putStr . show . parseAll . T.filter (not . isWhitespace)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n'

headMay :: T.Text -> Maybe Char
headMay t = if T.null t then Nothing else Just $ T.head t

-- We are going to ignore lexing for now, so all variable names will be single
-- letters in a-z.
data Expr = Var Char | App Expr Expr | Lam Char Expr deriving (Show)

data ParseResult = ParseResult { expr :: Expr, unparsedCode :: T.Text } deriving (Show)

parseVar :: T.Text -> Maybe ParseResult
parseVar code = do
  c <- headMay code
  if c >= 'a' && c <= 'z'
    then Just $ ParseResult (Var c) (T.tail code)
    else Nothing

parseApp :: T.Text -> Maybe ParseResult
parseApp code = do
  codeAfterOpenBracket <- consumeToken '(' code
  result <- parse codeAfterOpenBracket
  nextResult <- parse $ unparsedCode result
  remainingCode <- consumeToken ')' $ unparsedCode nextResult
  Just $ ParseResult (App (expr result) (expr nextResult)) remainingCode

parseLam :: T.Text -> Maybe ParseResult
parseLam code = do
  codeAfterOpenBracket <- consumeToken '(' code
  codeAfterLambda <- consumeToken '\\' codeAfterOpenBracket
  varResult <- parseVar codeAfterLambda
  codeAfterPeriod <- consumeToken '.' $ unparsedCode varResult
  exprResult <- parse codeAfterPeriod
  codeAfterCloseBracket <- consumeToken ')' $ unparsedCode exprResult
  case expr varResult of
    Var c -> Just $ ParseResult (Lam c (expr exprResult)) codeAfterCloseBracket
    _ -> Nothing

parse :: T.Text -> Maybe ParseResult
parse code = parseVar code <|> parseApp code <|> parseLam code

consumeToken :: Char -> T.Text -> Maybe T.Text
consumeToken token code = do
  headChar <- headMay code
  if headChar == token
    then Just $ T.tail code
    else Nothing

parseAll :: T.Text -> Maybe ParseResult
parseAll code = do
  result <- parse code
  if T.null $ unparsedCode result
    then Just result
    else Nothing
