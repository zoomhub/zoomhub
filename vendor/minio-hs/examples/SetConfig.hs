#!/usr/bin/env stack
-- stack --resolver lts-11.1 runghc --package minio-hs

--
-- MinIO Haskell SDK, (C) 2017, 2018 MinIO, Inc.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

{-# LANGUAGE OverloadedStrings #-}
import           Network.Minio
import           Network.Minio.AdminAPI

import           Prelude

main :: IO ()
main = do
  res <- runMinio def $
    do
      let config = "{\"version\":\"25\",\"credential\":{\"accessKey\":\"minio\",\"secretKey\":\"minio123\"},\"region\":\"\",\"browser\":\"on\",\"worm\":\"off\",\"domain\":\"\",\"storageclass\":{\"standard\":\"\",\"rrs\":\"\"},\"cache\":{\"drives\":[],\"expiry\":90,\"exclude\":[]},\"notify\":{\"amqp\":{\"2\":{\"enable\":false,\"url\":\"amqp://guest:guest@localhost:5672/\",\"exchange\":\"minio\",\"routingKey\":\"minio\",\"exchangeType\":\"direct\",\"deliveryMode\":0,\"mandatory\":false,\"immediate\":false,\"durable\":false,\"internal\":false,\"noWait\":false,\"autoDeleted\":false}},\"elasticsearch\":{\"1\":{\"enable\":false,\"format\":\"namespace\",\"url\":\"http://localhost:9200\",\"index\":\"minio_events\"}},\"kafka\":{\"1\":{\"enable\":false,\"brokers\":null,\"topic\":\"\"}},\"mqtt\":{\"1\":{\"enable\":false,\"broker\":\"\",\"topic\":\"\",\"qos\":0,\"clientId\":\"\",\"username\":\"\",\"password\":\"\",\"reconnectInterval\":0,\"keepAliveInterval\":0}},\"mysql\":{\"1\":{\"enable\":false,\"format\":\"namespace\",\"dsnString\":\"\",\"table\":\"\",\"host\":\"\",\"port\":\"\",\"user\":\"\",\"password\":\"\",\"database\":\"\"}},\"nats\":{\"1\":{\"enable\":false,\"address\":\"\",\"subject\":\"\",\"username\":\"\",\"password\":\"\",\"token\":\"\",\"secure\":false,\"pingInterval\":0,\"streaming\":{\"enable\":false,\"clusterID\":\"\",\"clientID\":\"\",\"async\":false,\"maxPubAcksInflight\":0}}},\"postgresql\":{\"1\":{\"enable\":false,\"format\":\"namespace\",\"connectionString\":\"\",\"table\":\"\",\"host\":\"\",\"port\":\"\",\"user\":\"\",\"password\":\"\",\"database\":\"\"}},\"redis\":{\"test1\":{\"enable\":true,\"format\":\"namespace\",\"address\":\"127.0.0.1:6379\",\"password\":\"\",\"key\":\"bucketevents_ns\"},\"test2\":{\"enable\":true,\"format\":\"access\",\"address\":\"127.0.0.1:6379\",\"password\":\"\",\"key\":\"bucketevents_log\"}},\"webhook\":{\"1\":{\"enable\":true,\"endpoint\":\"http://localhost:3000\"},\"2\":{\"enable\":true,\"endpoint\":\"http://localhost:3001\"}}}}"
      setConfig config
  print res
