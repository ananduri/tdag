module LearnParsers where

import Text.Trifecta
import Control.Applicative
import Data.Ratio ((%))

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

-- read two characters, '1' and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Integer -> IO ()
testParse p = print $
  parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

virtuousFraction :: Parser Rational
virtuousFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "denom is 0"
    _ -> return (numerator % denominator)

getInt :: Parser Integer
getInt = do
  int <- integer
  return int


type NumberOrString = Either Integer String

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)


main :: IO ()
main = do
  let p f i = parseString f mempty i
  print $ p parseNos "123abc456"

