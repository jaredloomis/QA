{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Control.Monad (void)
import Text.Parsec
import Text.Parsec.String

import QA hiding (testName)
import Element

parse :: Parser a -> String -> Either ParseError a
parse p = runParser p () "A Parser"

test :: Parser Test
test = do
    spaces
    name <- testName
    Test name <$> many (lexeme step) <* eof

testName :: Parser String
testName = lexeme (many alphaNum) <* lexeme (char ':')

-----------
-- Steps --
-----------

step :: Parser Step
step = try (fmap Act action) <|> fmap Assert assertion

action :: Parser Action
action = try goToUrl <|> try click <|> try execute <|> assign

goToUrl :: Parser Action
goToUrl = GoToUrl <$> (command "GoToUrl" *> quotedStr)

click :: Parser Action
click = Click <$> (command "Click" *> query)

assign :: Parser Action
assign = command "Assign" *> (Assign <$> query <*> lexeme quotedStr)

execute :: Parser Action
execute = Execute <$>
    (command "Execute" *> lexeme (string "{{") *>
    lexeme (manyTill anyChar (string "}}")))

assertion :: Parser Assertion
assertion = fmap TextExists (command "TextExists" *> quotedStr)

-------------
-- Helpers --
-------------

query :: Parser Element
query = Element <$> lexeme
    (between (string "$(") (string ")") (many (noneOf ")")))

command :: String -> Parser ()
command = void . lexeme . string

lexeme :: Parser a -> Parser a
lexeme = (<* spaces)

quotedStr :: Parser String
quotedStr = lexeme (between (char '"') (char '"') (many (noneOf "\"")))
