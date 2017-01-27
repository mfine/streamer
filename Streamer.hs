{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | AWS DynamoDB Streams
--
module Streamer
  ( listStreams
  , listShards
  , getIterator
  , listRecords
  , list
  ) where

import BasicPrelude hiding (empty)
import Turtle

-- | procStrict wrapper that throws exceptions.
--
process :: MonadIO m => Text -> [Text] -> Shell Text -> m Text
process cmd args i = do
  (ec, out) <- procStrict cmd args i
  case ec of
    ExitSuccess -> return out
    _           -> liftIO $ throwIO $ ProcFailed cmd args ec

-- | jq command.
--
jq :: MonadIO m => [Text] -> Shell Text -> m Text
jq args i = process "jq" ("-rc" : args) i

-- | aws command.
--
aws :: MonadIO m => Text -> [Text] -> m Text
aws cmd args = process "aws" (cmd : args) empty

-- | aws ddbstreams command.
--
ddbs :: MonadIO m => Text -> [Text] -> m Text
ddbs cmd args = aws "dynamodbstreams" (cmd : "--output" : "json" : args)

-- | aws ddbstreams list-streams command.
--
ddbsListStreams :: MonadIO m => Text -> m Text
ddbsListStreams table = ddbs "list-streams"
  [ "--table", table
  ]

-- | aws ddbstreams describe-stream command.
--
ddbsListShards :: MonadIO m => Text -> m Text
ddbsListShards stream = ddbs "describe-stream"
  [ "--stream-arn", stream
  ]

-- | aws ddbstreams get-shard-iterator command.
--
ddbsGetIterator :: MonadIO m => Text -> Text -> m Text
ddbsGetIterator stream shard = ddbs "get-shard-iterator"
  [ "--stream-arn", stream
  , "--shard-id", shard
  , "--shard-iterator-type", "LATEST"
  ]

-- | aws ddbstreams get-records command.
--
ddbsGetRecords :: MonadIO m => Text -> m Text
ddbsGetRecords iterator = ddbs "get-records"
  [ "--shard-iterator", iterator
  ]

-- | List steams for a table.
--
listStreams :: MonadIO m => Text -> m Text
listStreams table = do
  out <- ddbsListStreams table
  jq [ ".Streams[0].StreamArn" ] $ pure out

-- | List shards for a stream.
--
listShards :: MonadIO m => Text -> m Text
listShards stream = do
  out <- ddbsListShards stream
  jq [ ".StreamDescription.Shards[] | select ( .SequenceNumberRange | has (\"StartingSequenceNumber\") ) | . | select ( .SequenceNumberRange | has (\"EndingSequenceNumber\") | not ) | .ShardId" ] $ pure out

-- | Get iterator for a shard.
--
getIterator :: MonadIO m => Text -> Text -> m Text
getIterator stream shard = do
  out <- ddbsGetIterator stream shard
  jq [ ".ShardIterator" ] $ pure out

-- | List records for an interator.
--
listRecords :: MonadIO m => Text -> m Text
listRecords iterator = do
  out <- ddbsGetRecords iterator
  jq [ ".Records[].NewImage" ] $ pure out

-- | Combined operation.
--
list :: MonadIO m => Text -> m Text
list table = do
  streams <- lines <$> listStreams table
  fmap concat $ forM streams $ \stream -> do
    shards <- lines <$> listShards stream
    fmap concat $ forM shards $ \shard -> do
      iterator <- getIterator stream shard
      listRecords iterator

