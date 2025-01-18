import Data.Functor ((<&>))
import Data.Text as T (Text, unpack)
import Test.Hspec (describe, hspec, it, shouldBe)

import Hikchr (
  HikchrConfig (HikchrConfig, darkMode, height, svgClass, width),
  hikchr,
  hikchrCustom,
 )


pikchrScript :: Text
pikchrScript =
  "\
  \arrow right 200% \"Markdown\" \"Source\" \n\
  \box rad 10px \"Markdown\" \"Formatter\" \"(markdown.c)\" fit \n\
  \arrow right 200% \"HTML+SVG\" \"Output\" \n\
  \arrow <-> down 70% from last box.s \n\
  \box same \"Pikchr\" \"Formatter\" \"(pikchr.c)\" fit \n\
  \"


main :: IO ()
main = hspec $ do
  describe "Hikchr" $ do
    it "generates the expected SVG output" $ do
      svgResult <- hikchr pikchrScript

      expected <- readFile "test/expected.svg"
      (svgResult <&> T.unpack) `shouldBe` Right expected

    it "generates the expected SVG output with explicit config" $ do
      svgResult <-
        hikchrCustom
          ( HikchrConfig
              { svgClass = Just "example"
              , darkMode = False
              , width = Nothing
              , height = Nothing
              }
          )
          pikchrScript

      expected <- readFile "test/expected-class.svg"
      (svgResult <&> T.unpack) `shouldBe` Right expected

    it "generates an error message for invalid Pikchr input" $ do
      svgResult <- hikchr "invalid Pikchr input"

      (svgResult <&> T.unpack)
        `shouldBe` Left
          "/*    1 */  invalid Pikchr input\n\
          \                   ^^^^^^\nERROR: syntax error\n"
