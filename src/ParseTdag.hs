{-# LANGUAGE FlexibleContexts #-}

module ParseTDAG where

import Control.Applicative hiding ((<|>), many)

import Text.Parsec
import Text.ParserCombinators.Parsec hiding (try)

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
-- does this work with no content -> produce empty string?
p_content :: Parsec String () String
p_content = unwords <$> sepBy p_word p_sep





-- parse an actual node sexp
-- factor out this pattern of alternating p_seps?
-- this should only work at the top level (where we need a "node" keyword)
nodeParser :: Parsec String () Node
nodeParser = do
  p_sep
  char '(' *> string "node"
  p_sep
  id <- p_id
  p_sep
  content <- p_content <* char ')'
  bchild <- p_bchild
  return $ Node id content [] [bchild]
  


-- parse the inside of a node
-- the inside consists of:
-- content (which is always present, but could be empty (string)
-- zero or more sexps (each of which could occur more than once).
-- and the content and sexps could be in any order
p_inside :: Parsec String () (Content, [SExp])
p_inside = do
  list1 <- many p_sexp
  content <- p_content
  list2 <- many p_sexp
  return (content, [list1:list2])


p_sexp :: Parsec String () SExp
p_sexp = char '('
      *> (try p_bchild <?> "Tried to parse BChild")
     <|> (try p_props  <?> "Tried to parse Properties")
     <|> (p_parent     <?> "Tried to parse Parent")
      *> char ')'
-- parsing SExp, a sum type, is creating confusion


-- can you piece together datatypes using Applicatives?
-- yeah, gotta use fmap and stuff      

     

p_parent = do
  p_sep
  string "parent"
  p_sep
  ParentID <$> p_id

p_props = do
  p_sep
  string "props"
  p_sep
  unwords <$> sepBy p_word p_sep

-- parse a branch child (bchild) sexp
-- this is the same as node, except the keyword is "bchild"
-- these two need to be recursive, so yes you will need to parametrize node/bchild
p_bchild :: Parsec String () SExp
p_bchild = do
  p_sep
  string "bchild"
  p_sep
  id <- p_id
  p_sep
  content <- p_content <* char ')'
  return $ BChild $ Node id content [] []




data SExp = BChild Node
          | ParentID String
          | Properties String



-- define the node datatype here
-- should the list of child and parent nodes be
-- a list of ids, or a list of actual nodes?
data Node = Node ID Content [Node] [Node]
  deriving Show

type ID = String
type Content = String

