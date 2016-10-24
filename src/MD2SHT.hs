{-# LANGUAGE OverloadedStrings #-}

module MD2SHT where

import Data.Text hiding (concat, words, intercalate)
import MD2SHT.Types
import Text.HTML.TagSoup

extractStyles :: [Rule] -> [String] -> String
extractStyles rules classNames =
  concat $ applyRule =<< classNames
  where
    extractLine (Line (Property prop) (Value val)) =
      unpack prop ++ ":" ++ unpack val ++ ";"
    applyOnMatch match (Rule (Selector sel) ls) =
      if pack match `isInfixOf` sel
        then return $ concat $ extractLine <$> ls
        else mempty
    applyRule cn =
      applyOnMatch ("." ++ cn) =<< rules

replaceClassnames :: [Rule] -> String -> String
replaceClassnames rules html =
  renderTags $
    replaceClass <$>
    parseTags html
  where
    extractClassNames = words . fromAttrib "class"
    replaceClass tag@(TagOpen name _) = do
      let style = extractStyles rules $ extractClassNames tag
      TagOpen name [("style", style) | style /= ""]
    replaceClass tag = tag
