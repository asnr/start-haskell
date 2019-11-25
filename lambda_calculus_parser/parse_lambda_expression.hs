import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO()
main = TIO.getContents >>= putStr . show . parse . T.filter (not . isWhitespace)

isWhitespace :: Char -> Bool
isWhitespace c = c == ' ' || c == '\n'

-- We are going to ignore lexing for now, so all variable names will be single
-- letters in a-z. Lambdas will be represented by /.
data Expr = Var Char | App Expr Expr deriving (Show)

data ParseResult = ParseResult { expr :: Expr, unparsedCode :: T.Text } deriving (Show)

parseVar :: T.Text -> Maybe ParseResult
parseVar code = if T.head code >= 'a' && T.head code <= 'z'
  then Just $ ParseResult (Var (T.head code)) (T.tail code)
  else Nothing

parseApp :: T.Text -> Maybe ParseResult
parseApp code = let maybeResult = parseVar code in
  case maybeResult of
    Nothing -> Nothing
    Just result -> let maybeNextResult = parse $ unparsedCode result in
      case maybeNextResult of
        Nothing -> Nothing
        Just nextResult -> if T.null $ unparsedCode nextResult
          then Just $ ParseResult (App (expr result) (expr nextResult)) (unparsedCode nextResult)
          else Nothing

parse :: T.Text -> Maybe ParseResult
parse code = let varParse = parseVar code in
  case varParse of
    Nothing -> parseApp code
    Just result -> if T.null $ unparsedCode result
      then Just result
      else parseApp code
