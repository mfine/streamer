#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package streamer --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Streamer
import Turtle

parser :: Parser Text
parser = argText "stream" "Stream ARN"

main :: IO ()
main = do
  stream <- options "List Shards" parser
  out    <- listShards stream
  mapM_ putStrLn $ lines out
