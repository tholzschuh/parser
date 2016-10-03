module Json where

import Data.List (intercalate)
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

jsonString :: Parser JValue
jsonString = do
  char '"'
  str <- many (satisfy (\c -> c /= '\"'))
  char '"'
  return (JString str)

jsonNumber :: Parser JValue
jsonNumber = do
  pre  <- many digit
  char '.'
  post <- many digit
  return (JNumber (read (pre ++ "." ++ post)))

jsonBool = jsonTrue <|> jsonFalse
  where jsonTrue = do
          string "true"
          return (JBool True)
        jsonFalse = do
          string "false"
          return (JBool False)  

jsonNull :: Parser JValue
jsonNull = do
  str <- string "null"
  return JNull
