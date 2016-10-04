module Json where

import Data.List (intercalate)
import Control.Applicative
import Parser

type Identifier = String

data JValue = JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            | JObject [(Identifier, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)

render :: JValue -> String
render (JString str) = str
render (JNumber num) = show num
render (JBool True)  = "true"
render (JBool False) = "false"
render JNull         = "null"
render (JObject obj) = "{" ++ showObj obj ++ "}"
  where showObj [] = ""
        showObj xs = intercalate ", " (map renderPair xs)
        renderPair (k,v) = k ++ ": " ++ render v
render (JArray arr) = "[" ++ showArr arr ++ "]"
  where showArr [] = ""
        showArr xs = intercalate ", " (map render xs)

jsonParser :: Parser JValue
jsonParser =  jsonString
           <|> jsonNumber
           <|> jsonBool
           <|> jsonNull
           <|> jsonObject
           <|> jsonArray

jsonString :: Parser JValue
jsonString = do
  try (char '"')
  str <- many $ try (satisfy (/= '\"'))
  try (char '"')
  return (JString str)

jsonNumber :: Parser JValue
jsonNumber = do
  pre  <- many digit
  try (char '.')
  post <- many digit
  if null pre
    then return (JNull)
    else if null post
      then return (JNumber $ read pre)
      else return (JNumber $ read (pre ++ "." ++ post))

jsonBool :: Parser JValue
jsonBool = fmap JBool (try $ (string "true" >> return True) <|> (string "false" >> return False))

jsonNull :: Parser JValue
jsonNull = try (string "null") >> return JNull

jsonObject :: Parser JValue
jsonObject = do
  try (char '{')
  try tuple
  try (char '}')
  return JNull

tuple :: Parser (Identifier, JValue)
tuple = do
  char '('
  ident <- many (satisfy (/= ','))
  char ','
  value <- many (satisfy (/= ','))
  char ')'
  return (ident, JNull)

jsonArray :: Parser JValue
jsonArray = undefined
