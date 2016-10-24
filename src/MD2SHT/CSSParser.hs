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
      <|> fail "Didn't match */ to close comment"

skipSpace' :: Parser ()
skipSpace' =
  (skipComments >> skipSpace')
  <|> (skip isSpace >> skipSpace >> skipSpace')
  <|> return ()

parseProperty :: Parser Property
parseProperty = do
  p <- takeWhile (\x -> x /= ':' && x /= ' ')
  skipSpace'
  return $ Property (strip p)

parseValue :: Parser Value
parseValue = do
  v <- takeWhile (\x -> x /= ';' && x /= '}')
  skipSpace'
  return $ Value (strip v)

parseLine :: Parser Line
parseLine = do
  prop <- parseProperty
  skipSpace'
  void $ char ':'
  val <- parseValue
  skipSpace'
  void (char ';') <|> return ()
  skipSpace'
  return $ Line prop val

parseSelector :: Parser Selector
parseSelector = do
  sel <- takeWhile (/= '{')
  skipSpace'
  return $ Selector (strip sel)

parseRule :: Parser Rule
parseRule = do
  skipSpace'
  sel <- parseSelector
  void $ char '{'
  skipSpace'
  ls <- many' parseLine
  void $ char '}'
  skipSpace'
  return $ Rule sel ls

parseRules :: Parser [Rule]
parseRules = many' parseRule
