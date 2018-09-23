{-# LANGUAGE FlexibleContexts #-}

module ParseTDAG where

import Control.Applicative hiding ((<|>), many)

import Text.Parsec
import Text.ParserCombinators.Parsec

import TestData

myparse rule text = parse rule "" text

{-
want to parse things like
(node #0
 Why do we need databases?)

p_<thing> denotes a parser parsing some low-level object/token
-}

-- whitespace separator. newlines don't matter
p_sep :: Parsec String () String
p_sep = many (char ' ' <|> char '\n')

-- parses the "node" keyword
p_node :: Parsec String () String
p_node = string "node"

-- parses the "#<number>" identifier. may or may not be there
p_id :: Parsec String () String
p_id = char '#' >> many1 digit

-- parse a word
p_word :: Parsec String () String
p_word = many1 $ noneOf " ()"
--p_word = manyTill (many1 $ noneOf " ()") (char '(')

-- parse text. this will be the text part of the content of a node
-- only constraint is, it should not contain round parens
p_content :: Parsec String () String
p_content = unwords <$> sepBy p_word p_sep


-- parse an actual node sexp

-- can we factor out this pattern of alternating p_seps?
-- (the difference is, here what it's interweaved with is changing)

-- have to return an actual node data type now
nodeParser :: Parsec String () Node
nodeParser = do
  p_sep
  char '(' *> p_node
  p_sep
  id <- p_id
  p_sep
  content <- p_content <* char ')'
  bchild <- p_bchild
  return $ Node id content [] [bchild]




-- parse a branch child (bchild) sexp
-- this is the same as node, except the keyword is "bchild"
-- these two need to be recursive, so yes you will need to parametrize node/bchild
p_bchild :: Parsec String () Node
p_bchild = do
  p_sep
  char '(' *> string "node"
  p_sep
  id <- p_id   -- can we use the same variable name "id"?
  p_sep
  content <- p_content <* char ')'
  return $ Node id content [] []

  






-- define the node datatype here
-- should the list of child and parent nodes be
-- a list of ids, or a list of actual nodes?
data Node = Node ID Content [Node] [Node]
  deriving Show

type ID = String
type Content = String

