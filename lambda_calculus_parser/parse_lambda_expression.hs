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
parseVar code = let headChar = headMay code in
  case headChar of
    Nothing -> Nothing
    Just c -> if c >= 'a' && c <= 'z'
      then Just $ ParseResult (Var c) (T.tail code)
      else Nothing

parseApp :: T.Text -> Maybe ParseResult
parseApp code = let headChar = headMay code in
  case headChar of
    Nothing -> Nothing
    Just c -> if not (c == '(')
      then Nothing
      else let maybeResult = parse $ T.tail code in
        case maybeResult of
          Nothing -> Nothing
          Just result -> let maybeNextResult = parse $ unparsedCode result in
            case maybeNextResult of
              Nothing -> Nothing
              Just nextResult -> let lastHeadChar = headMay $ unparsedCode nextResult in
                case lastHeadChar of
                  Nothing -> Nothing
                  Just d -> if not (d == ')')
                    then Nothing
                    else Just $ ParseResult (App (expr result) (expr nextResult)) (T.tail $ unparsedCode nextResult)

parse :: T.Text -> Maybe ParseResult
parse code = let varParse = parseVar code in
  case varParse of
    Nothing -> parseApp code
    x -> x

parseAll :: T.Text -> Maybe ParseResult
parseAll code = let maybeResult = parse code in
  case maybeResult of
    Nothing -> Nothing
    Just result -> if T.null $ unparsedCode result
      then Just result
      else Nothing
