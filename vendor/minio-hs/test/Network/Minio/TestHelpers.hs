--
-- MinIO Haskell SDK, (C) 2018 MinIO, Inc.
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

module Network.Minio.TestHelpers
  ( runTestNS
  ) where

import           Network.Minio.Data

import           Lib.Prelude

newtype TestNS = TestNS { testNamespace :: Text }

instance HasSvcNamespace TestNS where
  getSvcNamespace = testNamespace

runTestNS :: ReaderT TestNS m a -> m a
runTestNS = flip runReaderT $
            TestNS "http://s3.amazonaws.com/doc/2006-03-01/"
