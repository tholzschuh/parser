module Parser where

import Control.Applicative hiding (many)
import Data.Char (isDigit, isLower, isUpper)

type Error = String
type ParseRes = Either Error

-- Parser
newtype Parser a = Parser { parse :: String -> (ParseRes a, String) }


-- INSTANTIATIONS
--
-- functor parsing interface
instance Functor Parser where
   --fmap :: (a -> b) -> Parser a -> Parser b
   fmap f p = Parser $ \inp -> case parse p inp of
     (Left err, str) -> (Left err, str)
     (Right x, str)  -> (Right (f x), str)

-- applicative functor parsing interface
instance Applicative Parser where
  --pure :: a -> Parser a
  pure x = Parser $ \inp -> (Right x, inp) -- always succeds, yields the argument as result without consuming input

  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pF <*> pA = Parser $ \inp -> case parse pF inp of
    (Left err, str) -> (Left err, str)
    (Right f, str)  -> case parse pA str of
      (Left err', str') -> (Left err', str')
      (Right x, str')   -> (Right (f x), str')

-- unexpected msg is a Parser that always fails with the Error 'msg' without consuming any input
unexpected :: String -> Parser a
unexpected err = Parser $ \inp -> (Left err, inp)

instance Alternative Parser where
  -- empty :: Parser a
  empty = unexpected "empty"

  -- (<|>) :: Parser a -> Parser a -> Parser a
  pX <|> pY = Parser $ \inp -> case parse pX inp of
    (Right x, str)  -> (Right x, str)
    (Left err, _)   -> case parse pY inp of
      (Right y, str')   -> (Right y, str')
      (Left err', _)    -> (Left err', inp)

-- p <?> msg: if p fails without consuming input, return msg.
(<?>) :: Parser a -> Error -> Parser a
p <?> msg = Parser $ \inp -> case parse p inp of
  (Right x, str)  -> (Right x, str)
  (Left err, str) -> if inp == str
                     then (Left msg, inp)
                     else (Left err, str)

-- monadic parsing interface
instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pA >>= f = Parser $ \inp -> case parse pA inp of
    (Left err, str) -> (Left err, str)
    (Right x, str)  -> parse (f x) str

-- generall purpose functions
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \inp -> case inp of
  []                 -> (Left "Empty string", [])
  (x:xs) | p x       -> (Right x, xs)
         | otherwise -> (Left "Did not satisfy", inp)

-- try p will return p's result if parsing p succeds, otherwise it will return the error message but consume _no_ input
try :: Parser a -> Parser a
try p = Parser $ \inp -> case parse p inp of
  (Left err, _)  -> (Left err, inp)
  (Right x, str) -> (Right x, str)

-- between open close p will parse open, then p, then close and yield the result of p
between :: Parser a -> Parser b -> Parser c -> Parser c
between open close p = do { open; x <- p; close; return x }

-- option x p will return x if p fails without consuming any input, otherwise it will return the result of p
option :: a -> Parser a -> Parser a
option x p = Parser $ \inp -> case parse p inp of
  (Right val, str) -> (Right val, str)
  (Left err, str)  -> if str == inp
                      then (Right x, inp)
                      else (Left err, str)

choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) empty ps

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: [Char] -> Parser Char
oneOf cs = choice (char <$> cs)

digit :: Parser Char
digit = satisfy isDigit

lower :: Parser Char
lower = satisfy isLower

upper :: Parser Char
upper = satisfy isUpper

letter :: Parser Char
letter = lower <|> upper

alphanum :: Parser Char
alphanum = letter <|> digit

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs
