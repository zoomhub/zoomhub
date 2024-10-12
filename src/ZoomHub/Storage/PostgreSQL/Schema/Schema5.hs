{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema5
  ( Schema5,
    Schemas5,
    migration,
  )
where

import Squeal.PostgreSQL
  ( Definition,
    IndexType (Btree),
    IsoQ (..),
    Join,
    Public,
    SchemumType (Index),
    SortExpression (Asc, AscNullsLast, Desc, DescNullsLast),
    btree,
    createIndex,
    dropIndex,
    dropIndexIfExists,
    (&),
    (:::),
    (>>>),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema4 (Schema4, Schemas4)

type Schema5 =
  Join
    Schema4
    '[ "content_active_at_idx" ::: 'Index 'Btree,
       "content_initialized_at_idx" ::: 'Index 'Btree,
       "content_num_views_idx" ::: 'Index 'Btree,
       "content_state_idx" ::: 'Index 'Btree,
       "content_submitter_email_idx" ::: 'Index 'Btree,
       "content_verified_at_idx" ::: 'Index 'Btree,
       "content_version_idx" ::: 'Index 'Btree
     ]

type Schemas5 = Public Schema5

migration :: Migration (IsoQ Definition) Schemas4 Schemas5
migration =
  Migration
    "2024-03-27-1: Recreate and expand content indexes"
    IsoQ
      { up = setup,
        down = teardown
      }
  where
    setup :: Definition Schemas4 Schemas5
    setup =
      -- Clean up of unsafe legacy index definition in `Schema4`:
      dropIndexIfExists #content_active_at_idx
        >>> dropIndexIfExists #content_initialized_at_idx
        >>> dropIndexIfExists #content_num_views_idx
        >>> dropIndexIfExists #content_state_idx
        >>> dropIndexIfExists #content_verified_at_idx
        >>> dropIndexIfExists #content_version_idx
        -- new type-safe index creation
        >>> createIndex #content_active_at_idx #content btree [#active_at & DescNullsLast]
        >>> createIndex #content_initialized_at_idx #content btree [#initialized_at & Desc]
        >>> createIndex #content_num_views_idx #content btree [#num_views & Desc]
        >>> createIndex #content_state_idx #content btree [#state & Asc]
        >>> createIndex #content_submitter_email_idx #content btree [#submitter_email & AscNullsLast]
        >>> createIndex #content_verified_at_idx #content btree [#verified_at & DescNullsLast]
        >>> createIndex #content_version_idx #content btree [#version & Desc]

    teardown :: Definition Schemas5 Schemas4
    teardown =
      dropIndex #content_active_at_idx
        >>> dropIndex #content_initialized_at_idx
        >>> dropIndex #content_num_views_idx
        >>> dropIndex #content_state_idx
        >>> dropIndex #content_submitter_email_idx
        >>> dropIndex #content_verified_at_idx
        >>> dropIndex #content_version_idx
