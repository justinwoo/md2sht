{-# LANGUAGE OverloadedStrings #-}

module MD2SHT.CSSParser where

import Prelude hiding (takeWhile, lines)

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text hiding (takeWhile)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Char

import MD2SHT.Types

skipComments :: Parser ()
skipComments =
  string "/*" >> closeComment
  where
    closeComment =
      skipWhile (/= '*') >> string "*/" >> return ()
      <|> closeComment

skipSpace' :: Parser ()
skipSpace' =
  (skipComments >> skipSpace')
  <|> (takeWhile1 isSpace >> skipSpace')
  <|> return ()

lexeme :: Parser a -> Parser a
lexeme p = p <* skipSpace'

parseProperty :: Parser Property
parseProperty = lexeme $
  Property . strip <$> takeWhile (\x -> x /= ':' && x /= ' ')

parseValue :: Parser Value
parseValue = lexeme $
  Value . strip <$> takeWhile (\x -> x /= ';' && x /= '}')

parseLine :: Parser Line
parseLine = lexeme $ do
  prop <- parseProperty
  void $ char ':'
  val <- parseValue
  void (char ';') <|> return ()
  return $ Line prop val

parseSelector :: Parser Selector
parseSelector = lexeme $
  Selector . strip <$> takeWhile (/= '{')

parseRule :: Parser Rule
parseRule = do
  skipSpace'
  sel <- parseSelector
  void $ lexeme (char '{')
  ls <- many' parseLine
  void $ lexeme (char '}')
  return $ Rule sel ls

parseRules :: Parser [Rule]
parseRules = many' parseRule
