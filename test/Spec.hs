import Hikchr
import Data.Text as T
import Test.Hspec

pikchrDiagram :: Text
pikchrDiagram = "\
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
      svg <- hikchr $ HikchrConfig
        { diagram = pikchrDiagram
        , svgClass = "example"
        , renderFlags = 0
        , width = Just 500
        , height = Just 400
        }

      expected <- readFile "test/expected.svg"
      T.unpack svg `shouldBe` expected
