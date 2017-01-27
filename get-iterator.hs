#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package streamer --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Streamer
import Turtle

parser :: Parser (Text, Text)
parser = (,)
  <$> argText "stream" "Stream ARN"
  <*> argText "shard" "Shard Id"

main :: IO ()
main = do
  (stream, shard) <- options "List Shards" parser
  out             <- getIterator stream shard
  mapM_ putStrLn $ lines out
