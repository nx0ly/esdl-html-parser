module Convert where

import qualified Markup
import qualified Html

convertStructure :: Markup.Structure -> Html.Structure
convertStructure s =
    case s of
        Markup.Heading n txt -> 
            Html.h_ n txt

        Markup.Paragraph txt -> 
            Html.p_ txt
        
        Markup.UnorderedList list -> 
            Html.ul_ $ map Html.p_ list
        
        Markup.OrderedList list -> 
            Html.ol_ $ map Html.p_ list

        Markup.CodeBlock list ->
            Html.code_ (unlines list)

mconcat :: Monoid a => [a] -> a
mconcat list =
  case list of
    [] -> mempty
    x : xs -> x <> Html.mconcat xs

convert :: Html.Title -> Markup.Document -> Html.Html
convert title = Html.html_ title . foldMap convertStructure
