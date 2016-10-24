{-# LANGUAGE OverloadedStrings #-}

module MD2SHT.CSSParser where

import Prelude hiding (takeWhile, lines)

import Control.Applicative ((<|>))
import Control.Monad (void)
import Data.Text hiding (takeWhile)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

import MD2SHT.Types

skipComments :: Parser ()
skipComments = do
  void (string "/*") <|> return ()
  skipWhile $ (/=) '*'
  void (string "*/")
  return ()

skipSpace' :: Parser ()
skipSpace' = do
  skipSpace
  skipComments <|> return ()

parseProperty :: Parser Property
parseProperty = do
  skipSpace'
  p <- takeWhile (\x -> x /= ':' && x /= ' ')
  return $ Property (strip p)

parseValue :: Parser Value
parseValue = do
  skipSpace'
  v <- takeWhile (\x -> x /= ';' && x /= '}')
  return $ Value (strip v)

parseLine :: Parser Line
parseLine = do
  skipSpace'
  prop <- parseProperty
  skipSpace'
  void $ char ':'
  val <- parseValue
  skipSpace'
  void (char ';') <|> return ()
  return $ Line prop val

parseSelector :: Parser Selector
parseSelector = do
  skipSpace'
  sel <- takeWhile $ (/=) '{'
  return $ Selector (strip sel)

parseRule :: Parser Rule
parseRule = do
  skipSpace'
  sel <- parseSelector
  skipSpace'
  void $ char '{'
  skipSpace'
  ls <- many' parseLine
  skipSpace'
  void $ char '}'
  return $ Rule sel ls

parseRules :: Parser [Rule]
parseRules = many' parseRule
