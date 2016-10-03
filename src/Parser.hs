module Parser where

import Control.Applicative hiding (many)
import Data.Char (isDigit, isLower, isUpper)

type Error = String
type ParseRes a = Either Error a

-- Parser
newtype Parser a = Parser { parse :: String -> (ParseRes a, String) }


-- INSTANTIATIONS
--
-- functor parsing interface
instance Functor Parser where
   --fmap :: (a -> b) -> Parser a -> Parser b
   fmap f (Parser p) = Parser $ \inp -> case p inp of
     (Left err, str) -> (Left err, str)
     (Right x, str)  -> (Right (f x), str)

-- applicative functor parsing interface
instance Applicative Parser where
  --pure :: a -> Parser a
  pure x = Parser $ \inp -> (Right x, inp)

  --(<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser ff) <*> (Parser xx) = Parser $ \inp -> case ff inp of
    (Left err, str) -> (Left err, str)
    (Right f, str)  -> case xx str of
      (Left err', str') -> (Left err', str')
      (Right x, str')   -> (Right (f x), str')

instance Alternative Parser where
  empty = Parser $ \inp -> (Left "empty", inp)

  (<|>) (Parser xx) (Parser yy) = Parser $ \inp -> case xx inp of
    (Right x, str)  -> (Right x, str)
    (Left err, _)   -> case yy inp of
      (Right y, str')   -> (Right y, str')
      (Left err', _)    -> (Left err', inp)



-- monadic parsing interface
instance Monad Parser where
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser xx) >>= ff = Parser $ \inp -> case xx inp of
    (Left err, str) -> (Left err, str)
    (Right x, str)  -> parse (ff x) str

zero :: Parser a
zero = Parser $ \inp -> (Left "zero", inp)

-- generall purpose functions
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \inp -> case inp of
  []                     -> (Left "empty string", [])
  (x:xs) | p x       -> (Right x, xs)
         | otherwise -> (Left "did not satisfy", inp)

try :: Parser a -> Parser a
try (Parser p) = Parser $ \inp -> case p inp of
  (Left err, _)  -> (Left err, inp)
  (Right x, str) -> (Right x, str)

--many :: Parser a -> Parser [a]
--many (Parser f) = Parser loop
--    where
--      loop = \inp -> case f inp of
--        (Left _, _)     -> (Right [], inp)
--        (Right x, str)  -> case loop str of
--          (Left _, _)       -> (Right [x], str)
--          (Right x', str')  -> (Right (x:x'), str')

--some :: Parser a -> Parser [a]
--some (Parser f) = Parser loop
--    where
--      loop = \inp -> case f inp of
--        (Left err, _)   -> (Left err, inp)
--        (Right x, str)  -> case loop str of
--         (Left _, _)       -> (Right [x], str)
--          (Right x', str')  -> (Right (x:x'), str')

choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) zero ps

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
string []     = return []
string (c:cs) = do
  x  <- char   c
  xs <- string cs
  return (x:xs)

-- only use Applicative
--string' :: String -> Parser String
--string' [] = pure []
--string' (c:cs) = (:) <$> char c <*> string' cs
