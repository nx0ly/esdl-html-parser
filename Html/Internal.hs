module Html.Internal where

import Numeric.Natural (Natural)

newtype Html = Html String

newtype Structure = Structure String

type Title = String

html_ :: Title -> Structure -> Html
html_ title content = Html (el "html" (el "head" (el "title" title) <> el "body" (getStructureString content)))

body_ :: String -> String
body_ = el "body"

title_ :: String -> String
title_ title = "<head><title>" <> title <> "</title></head>"

p_ :: String -> Structure
p_ = Structure . el "p" . escape

customTag :: String -> String -> Structure
customTag tag = Structure . el tag . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concatMap (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concatMap (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

empty_ :: Structure
empty_ = Structure ""

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = Structure ""


render :: Html -> String
render html = case html of
  Html str -> str

getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concatMap escapeChar
