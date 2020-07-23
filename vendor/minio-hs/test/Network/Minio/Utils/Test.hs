--
-- MinIO Haskell SDK, (C) 2017 MinIO, Inc.
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

module Network.Minio.Utils.Test
  (
    limitedMapConcurrentlyTests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Lib.Prelude

import           Network.Minio.Utils

limitedMapConcurrentlyTests :: TestTree
limitedMapConcurrentlyTests = testGroup "limitedMapConcurrently Tests"
  [ testCase "Test with various thread counts" testLMC
  ]

testLMC :: Assertion
testLMC = do
  let maxNum = 50
  -- test with thread count of 1 to 2*maxNum
  forM_ [1..(2*maxNum)] $ \threads -> do
    res <- limitedMapConcurrently threads compute [1..maxNum]
    sum res @?= overallResultCheck maxNum
  where
    -- simple function to run in each thread
    compute :: Int -> IO Int
    compute n = return $ sum [1..n]

    -- function to check overall result
    overallResultCheck n = sum $ map (\t -> (t * (t+1)) `div` 2) [1..n]
