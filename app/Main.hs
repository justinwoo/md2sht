{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Attoparsec.Text
import Data.Function ((&))
import Data.Maybe
import Data.Text
import MD2SHT
import MD2SHT.CSSParser
import Options.Generic
import Text.Pandoc hiding (def)
import Text.Pandoc.Error
import qualified Data.Set as Set
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
  Html . writeHtmlString writeOpts <$> readMarkdown readOpts md
  where
  writeOpts = PO.def
    { writerHighlight = True
    , writerHtml5 = True
    }
  readOpts = PO.def
    { readerExtensions = githubMarkdownExtensions
        & Set.delete PO.Ext_hard_line_breaks
    }


main :: IO ()
main = do
  opts <- getRecord "md2sht -- convert markdown to inline-styled html"
  -- normalize opts
  let stylesheet' = fromMaybe "default.css" (unHelpful $ stylesheet opts)
  let writer = maybe putStrLn writeFile $ unHelpful $ output opts
  -- CSS:
  parsedCss <- parseOnly parseRules . pack <$> readFile stylesheet'
  rules <- either error pure parsedCss

  -- HTML
  converted <- md2html . Markdown <$> readFile (unHelpful $ input opts)
  Html html <- either (error . show) pure converted

  -- Combine
  let result = replaceClassnames rules html
  writer result
