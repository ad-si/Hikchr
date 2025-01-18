# Hikchr

Hikchr is a Haskell wrapper for [Pikchr](https://pikchr.org/),
a PIC-like markup language for diagrams.

For example, the following Haskell code:

```haskell
import Hikchr

main :: IO ()
main = do
  let pikchrDiagram = """
        arrow right 200% "Markdown" "Source"
        box rad 10px "Markdown" "Formatter" "(markdown.c)" fit
        arrow right 200% "HTML+SVG" "Output"
        arrow <-> down 70% from last box.s
        box same "Pikchr" "Formatter" "(pikchr.c)" fit
      """

  svg <- hikchr
      { diagram = pikchrDiagram
      , svgClass = "example"
      , renderFlags = []
      , width = Just 500
      , height = Just 400
      }

  writeFile "example.svg" svg
```

will generate following SVG:

![](example.svg)
