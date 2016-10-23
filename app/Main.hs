{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Attoparsec.Text
import Data.Maybe
import Data.Text
import MD2SHT
import MD2SHT.CSSParser
import Options.Generic
import Text.Pandoc hiding (def)
import Text.Pandoc.Error
import qualified Text.Pandoc.Options as PO

data MD2SHT = MD2SHT
  { input :: FilePath <?> "Input markdown file"
  , output :: Maybe FilePath <?> "Optional output html filepath -- defaults to standard out"
  , stylesheet :: Maybe FilePath <?> "Optional input stylesheet filepath -- defaults to 'default.css'"
  } deriving (Generic, Show)

instance ParseRecord MD2SHT

newtype Markdown = Markdown String
  deriving (Show)
newtype Html = Html String
  deriving (Show)

md2html :: Markdown -> Either PandocError Html
md2html (Markdown md) =
  Html . writeHtmlString (PO.def {writerHighlight = True, writerHtml5 = True}) <$> readMarkdown PO.def md

main :: IO ()
main = do
  opts <- getRecord "md2sht -- convert markdown to inline-styled html"
  let stylesheet' = fromMaybe "default.css" (unHelpful $ stylesheet opts)
  html <- md2html . Markdown <$> readFile (unHelpful $ input opts)
  css <- parseOnly parseRules . pack <$> readFile stylesheet'
  case (html, css) of
    (Left e, _) -> error (show e)
    (_, Left e) -> error e
    (Right (Html html'), Right rules) -> do
      let result = replaceClassnames rules html'
      case unHelpful $ output opts of
        Nothing -> putStrLn result
        Just path -> do
          writeFile path result
          putStrLn $ "output written to " ++ path
