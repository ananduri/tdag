{-# LANGUAGE QuasiQuotes #-}

module TestData where

import Text.RawString.QQ

test0 :: String
test0 = [r|(node #0
Why do we need databases?)
|]



test1 :: String
test1 = [r|
(node #0
 Why do we need databases?)

(node #1
 We need to hold on to data)
|]


test2 :: String
test2 = [r|
(node #2
 Sometimes we have more data than can fit in memory.
 (bchild #3
  What is memory?))
|]


  
