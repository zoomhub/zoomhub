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

module Lib.Prelude
    ( module Exports
    , both
    ) where

import           Protolude                 as Exports hiding (catch, catches,
                                                       throwIO, try)

import           Control.Monad.Trans.Maybe as Exports (MaybeT (..), runMaybeT)
import           Data.Time                 as Exports (UTCTime (..),
                                                       diffUTCTime)
import           UnliftIO                  as Exports (catch, catches, throwIO,
                                                       try)

-- | Apply a function on both elements of a pair
both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
