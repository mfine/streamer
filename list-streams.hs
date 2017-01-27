#!/usr/bin/env stack
-- stack runghc --package basic-prelude --package streamer --package turtle

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import BasicPrelude
import Streamer
import Turtle

parser :: Parser Text
parser = argText "table" "Table Name"

main :: IO ()
main = do
  table <- options "List Streams" parser
  out   <- listStreams table
  mapM_ putStrLn $ lines out
