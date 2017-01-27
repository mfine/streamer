#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package streamer --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Streamer
import Turtle

parser :: Parser Text
parser = argText "iterator" "Shard Iterator"

main :: IO ()
main = do
  iterator <- options "List Records" parser
  out      <- listRecords iterator
  mapM_ putStrLn $ lines out
