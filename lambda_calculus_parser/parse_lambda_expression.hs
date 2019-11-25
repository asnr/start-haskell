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
data Expr = Var Char | App Expr Expr deriving (Show)

data ParseResult = ParseResult { expr :: Expr, unparsedCode :: T.Text } deriving (Show)

parseVar :: T.Text -> Maybe ParseResult
parseVar code = do {
  c <- headMay code;
  if c >= 'a' && c <= 'z'
    then Just $ ParseResult (Var c) (T.tail code)
    else Nothing
  }

parseApp :: T.Text -> Maybe ParseResult
parseApp code = do {
  codeAfterOpenBracket <- consumeToken '(' code;
  result <- parse codeAfterOpenBracket;
  nextResult <- parse $ unparsedCode result;
  remainingCode <- consumeToken ')' $ unparsedCode nextResult;
  Just $ ParseResult (App (expr result) (expr nextResult)) remainingCode
  }

parse :: T.Text -> Maybe ParseResult
parse code = let varParse = parseVar code in
  case varParse of
    Nothing -> parseApp code
    x -> x

consumeToken :: Char -> T.Text -> Maybe T.Text
consumeToken token code = do {
  headChar <- headMay code;
  if headChar == token
    then Just $ T.tail code
    else Nothing
  }

parseAll :: T.Text -> Maybe ParseResult
parseAll code = let maybeResult = parse code in
  case maybeResult of
    Nothing -> Nothing
    Just result -> if T.null $ unparsedCode result
      then Just result
      else Nothing
