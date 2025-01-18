import Hikchr

main :: IO ()
main = do
  let pikchrDiagram = "\
        \arrow right 200% "Markdown" "Source" \n\
        \box rad 10px "Markdown" "Formatter" "(markdown.c)" fit \n\
        \arrow right 200% "HTML+SVG" "Output" \n\
        \arrow <-> down 70% from last box.s \n\
        \box same "Pikchr" "Formatter" "(pikchr.c)" fit \n\
        \"

  svg <- hikchr
      { diagram = pikchrDiagram
      , svgClass = "example"
      , renderFlags = []
      , width = Just 500
      , height = Just 400
      }

  writeFile "test_output.svg" svg
