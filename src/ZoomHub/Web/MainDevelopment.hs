{-# LANGUAGE OverloadedStrings #-}

-- Copyright (c) 2015 Matt Parsons
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- a copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to
-- permit persons to whom the Software is furnished to do so, subject to
-- the following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-- See: https://github.com/parsonsmatt/servant-persistent/commit/95df92b2fe9b0f421afa0cf1bcc9c3a4ca38b48c
module ZoomHub.Web.MainDevelopment where -- Originally `DevelMain`

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Typeable
import Foreign.Store (Store (..), lookupStore, readStore, storeAction, withStore)
import GHC.Word (Word32)
import Say
import System.IO
import qualified ZoomHub.Web.Main as Web
import Prelude

tshow :: (Show a) => a -> Text
tshow = Text.pack . show

runAppDevelopment :: IO ()
runAppDevelopment = Web.webMain

-- | Start or restart the server.
-- A Store holds onto some data across ghci reloads
update :: IO ()
update = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    Nothing -> do
      say "no server running"
      done <- storeAction doneStore newEmptyMVar
      tid <- start done
      _ <- storeAction (Store tidStoreNum) (newIORef tid)
      return ()
    Just tidStore -> do
      say "restarting app..."
      restartAppInNewThread tidStore
  where
    doneStore :: Store (MVar ())
    doneStore = Store 0

    -- shut the server down with killThread and wait for the done signal
    restartAppInNewThread :: Store (IORef ThreadId) -> IO ()
    restartAppInNewThread tidStore = modifyStoredIORef tidStore $ \tid -> do
      say $ "killing thread: " <> tshow tid
      killThread tid
      say "taking mvar"
      withStore doneStore takeMVar
      readStore doneStore >>= start

    start ::
      MVar () ->
      IO ThreadId
    start done =
      myThreadId
        <* ( do
               say "in forkFinally"
               runAppDevelopment `catch` \(SomeException e) -> do
                 say "!!! exception in runAppDevelopment !!!"
                 say $ "X    exception type: " <> tshow (typeOf e)
                 say $ "X    exception     : " <> tshow e
               say "runAppDevelopment terminated"
           )
          `catch` ( \(SomeException err) -> do
                      say "catch action"
                      hFlush stdout
                      hFlush stderr
                      putMVar done ()
                      say $ "Got Exception: " <> tshow err
                      throwIO err
                  )
          `finally` ( do
                        say "finally action"
                        hFlush stdout
                        hFlush stderr
                        putMVar done ()
                    )

-- | kill the server
shutdown :: IO ()
shutdown = do
  mtidStore <- lookupStore tidStoreNum
  case mtidStore of
    -- no server running
    Nothing -> say "no app running"
    Just tidStore -> do
      withStore tidStore $ readIORef >=> killThread
      say "App is shutdown"

tidStoreNum :: Word32
tidStoreNum = 1

modifyStoredIORef :: Store (IORef a) -> (a -> IO a) -> IO ()
modifyStoredIORef store f = withStore store $ \ref -> do
  v <- readIORef ref
  f v >>= writeIORef ref
