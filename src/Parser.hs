module Parser where

import Control.Applicative
import Control.Monad

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
      (Right x, str') -> (Right (f x), str')

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \inp -> (Left "empty", inp)

  --(<|>) :: Parser a -> Parser a -> Parser a
  (Parser xx) <|> (Parser yy) = Parser $ \inp -> case xx inp of
    (Right x, str)  -> (Right x, str)
    (Left err, str)   -> case yy inp of
      (Right y, str')   -> (Right y, str')
      (Left err', str') -> (Left err', inp)

  --many :: Parser a -> Parser [a]
  many (Parser f) = Parser loop
    where
      loop = \inp -> case f inp of
        (Left err, _) -> (Right [], inp)
        (Right x, str)  -> case loop str of
          (Left err', str') -> (Right [x], str)
          (Right x', str')  -> (Right (x:x'), str')

  --some :: Parser a -> Parser [a]
  some (Parser f) = Parser loop
    where
      loop = \inp -> case f inp of
        (Left err, str) -> (Left err, inp)
        (Right x, str)  -> case loop str of
          (Left err', str') -> (Right [x], str)
          (Right x', str')  -> (Right (x:x'), str')

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
  []                 -> (Left "empty string", [])
  inp@(x:xs) | p x       -> (Right x, xs)
         | otherwise -> (Left "did not satisfy", inp)

try :: Parser a -> Parser a
try (Parser p) = Parser $ \inp -> case p inp of
  (Left err, _)  -> (Left err, inp)
  (Right x, str) -> (Right x, str)



choice :: [Parser a] -> Parser a
choice ps = foldr (<|>) zero ps

char :: Char -> Parser Char
char c = satisfy (== c)

oneOf :: [Char] -> Parser Char
oneOf cs = choice (char <$> cs)

digit :: Parser Char
digit = satisfy (\x -> x >= '0' && x <= '9')

lower :: Parser Char
lower = satisfy (\x -> x >= 'a' && x <= 'z')

upper :: Parser Char
upper = satisfy (\x -> x >= 'A' && x <= 'Z')

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
