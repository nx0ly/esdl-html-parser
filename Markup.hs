module Markup where

import Data.Maybe (maybeToList)
import Numeric.Natural
import Prelude hiding (even, odd)

type Document = [Structure]

data Structure = Heading Natural String | Paragraph String | UnorderedList [String] | OrderedList [String] | CodeBlock [String] deriving (Show)

parse :: String -> Document
parse = parseLines Nothing . lines -- (1)

parseLines :: Maybe Structure -> [String] -> Document
parseLines ctx txts =
  case txts of
    [] -> maybeToList ctx -- (2)

    -- heading
    ('*' : ' ' : line) : rest ->
      maybe id (:) ctx (Heading 1 (trim line) : parseLines Nothing rest)
    -- unordered list
    ('-' : ' ' : line) : rest ->
      case ctx of
        Just (UnorderedList list) ->
          parseLines (Just (UnorderedList (list <> [trim line]))) rest
        _ -> maybe id (:) ctx (parseLines (Just (UnorderedList [trim line])) rest)
    -- ordered list
    ('~' : ' ' : line) : rest ->
      case ctx of
        Just (OrderedList list) ->
          parseLines (Just (OrderedList (list <> [trim line]))) rest
        _ -> maybe id (:) ctx (parseLines (Just (OrderedList [trim line])) rest)
    -- codeblocks
    ('>' : ' ' : line) : rest ->
      case ctx of
        Just (CodeBlock code) ->
          parseLines (Just (CodeBlock (code <> [line]))) rest
        _ -> maybe id (:) ctx (parseLines (Just (CodeBlock [line])) rest)
    -- paragraph
    currentLine : rest ->
      let line = trim currentLine
       in if line == ""
            then maybe id (:) ctx (parseLines Nothing rest) -- (3)
            else case ctx of
              Just (Paragraph prgh) ->
                parseLines (Just (Paragraph (unwords [prgh, line]))) rest -- (4)
              _ ->
                maybe id (:) ctx (parseLines (Just (Paragraph line)) rest)

trim :: String -> String
trim = unwords . words

print :: (Show a) => a -> IO ()
print = putStrLn . show
