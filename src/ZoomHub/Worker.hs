{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZoomHub.Worker
  ( processExistingContent,
    processExpiredActiveContent,
  )
where

import ZoomHub.Config (Config (..))

-- Public API
processExistingContent :: Config -> String -> IO ()
processExistingContent Config {..} _workerId = pure ()

processExpiredActiveContent :: Config -> IO ()
processExpiredActiveContent Config {..} = pure ()
