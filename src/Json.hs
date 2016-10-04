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

json = jsonParser

jsonParser :: Parser JValue
jsonParser =  space >>
          (jsonString
          <|> jsonNumber
          <|> jsonBool
          <|> jsonNull
          <|> jsonObject
          <|> jsonArray)


jsonString :: Parser JValue
jsonString = JString <$> (try $ between open close p)
  where open  = (char '"') <?> "expected \'\"\'"
        close = open
        p     = many (satisfy (/= '"'))

jsonNumber :: Parser JValue
jsonNumber = double <|> integer
  where
    double = do
      pre  <- some digit
      char '.'
      post <- some digit
      return (JNumber (read (pre ++ "." ++ post)))
    integer = do
      num <- some digit
      return (JNumber (read num))


jsonBool :: Parser JValue
jsonBool = JBool <$> (try $ (string "true" >> return True) <|> (string "false" >> return False))

jsonNull :: Parser JValue
jsonNull = try $ string "null" >> return JNull

jsonObject :: Parser JValue
jsonObject = between open close p
  where open  = (char '{' <?> "expected \'{\'") >> space
        close = space >> (char '}' <?> "expected \'}'\'")
        p = do
          content <- parseObject
          return (JObject content)

        parseObject = do
          obj  <- parseSingle
          rest <- many (space >> (char ',') >> space >> parseSingle)
          return (obj:rest)

        parseSingle = do
          (JString ident) <- jsonString
          space >> char ':' <?> "expected \':\'"
          value           <- jsonParser
          return (ident, value)

jsonArray :: Parser JValue
jsonArray = between open close p
  where open  = (char '[' <?> "expected \'[\'") >> space
        close = space >> (char ']' <?> "expected \']\'")
        p = do
          elements <- parseArray
          return (JArray elements)
        parseArray = do
          start <- jsonParser
          rest  <- many (space >> (char ',') >> jsonParser)
          return (start:rest)
