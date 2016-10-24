{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

import Test.Hspec
import Data.Attoparsec.Text
import Data.Text
import MD2SHT
import MD2SHT.CSSParser
import MD2SHT.Types

testRules :: Text
testRules = "\
\ .hll { background-color: #ffffcc } /* Crap */ \
\ .c { color: #408080; font-style: italic; } /* Comment */ \
\ .err { border: 1px solid #FF0000 } /* Error */ \
\ "

testRule :: Text
testRule = ".hll { background-color: #ffffcc }"

testRule2 :: Text
testRule2 = ".c { color: #408080; font-style: italic }"

testRule3 :: Text
testRule3 = ".c {  } /* sdfsdf * * sd */ /* sdfsdf * * sd */"

testLine :: Text
testLine = "background-color: #ffffcc"

testParse' :: forall a. (Show a, Eq a) => Parser a -> Text -> a -> Expectation
testParse' parser testData expected =
  case parseOnly parser testData of
    Right result -> result `shouldBe` expected
    Left msg -> error msg

main :: IO ()
main = hspec $ do
  describe "CSSParser" $ do
    it "parses a line" $
      testParse' parseLine testLine $
        Line (Property "background-color") (Value "#ffffcc")
    it "parses a rule" $
      testParse' parseRule testRule $
        Rule (Selector ".hll")
          [ Line (Property "background-color") (Value "#ffffcc") ]
    it "parses a rule with multiple lines" $
      testParse' parseRule testRule2 $
        Rule (Selector ".c")
          [ Line (Property "color") (Value "#408080")
          , Line (Property "font-style") (Value "italic") ]
    it "parses multiple rules with simple comments" $
      testParse' parseRules testRules
        [ Rule (Selector ".hll")
            [ Line (Property "background-color") (Value "#ffffcc") ]
        , Rule (Selector ".c")
            [ Line (Property "color") (Value "#408080")
            , Line (Property "font-style") (Value "italic") ]
        , Rule (Selector ".err")
            [ Line (Property "border") (Value "1px solid #FF0000") ]
        ]
    it "parses a rule with silly comments" $
      testParse' parseRule testRule3 $
        Rule (Selector ".c") []
  describe "extractStyles" $
    case parseOnly parseRules testRules of
      Left msg -> error msg
      Right rules -> do
        it "extracts out a style as a flattened string" $
          extractStyles rules ["hll", "c"] `shouldBe`
            "background-color:#ffffcc;color:#408080;font-style:italic;"
        it "should generate a tag with a style attribute" $
          replaceClassnames rules "<p class=\"c hll\">" `shouldBe`
            "<p style=\"color:#408080;font-style:italic;background-color:#ffffcc;\">"
