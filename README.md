# Hikchr

Hikchr is a Haskell wrapper for [Pikchr](https://pikchr.org/),
a PIC-like markup language for diagrams.


## Library

Install by adding `hikchr` to your `*.cabal` file
and use the `hikchr` functions to convert Pikchr scripts to SVG:

```haskell
import Hikchr

main :: IO ()
main = do
  let pikchrScript = """
        arrow right 200% "Markdown" "Source"
        box rad 10px "Markdown" "Formatter" "(markdown.c)" fit
        arrow right 200% "HTML+SVG" "Output"
        arrow <-> down 70% from last box.s
        box same "Pikchr" "Formatter" "(pikchr.c)" fit
      """

  svgResult <- hikchrCustom
      ( HikchrConfig
          { svgClass = Just "example"
          , darkMode = False
          , width = Nothing
          , height = Nothing
          }
      )
      pikchrScript

  -- Without configuration:
  -- svgResult <- hikchr pikchrScript

  case svgResult of
    Left err -> putErrText err
    Right svg -> writeFile "example.svg" svg
```

will generate following SVG:

![](example.svg)


## CLI

Install by running `cabal install hikchr` or `stack install hikchr`
and use the `hikchr` command like this:

```shell
hikchr graph.pikchr > graph.svg

echo "box \"Hello\"" | hikchr > hello.svg

hikchr --dark-mode graph.pikchr > graph-dark.svg

hikchr --class graph graph.pikchr > graph.svg

hikchr example1.pikchr example2.pikchr \
| cat "<html><body>" - "</body></html>" \
> example.html
```
