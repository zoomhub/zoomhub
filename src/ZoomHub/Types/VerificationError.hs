module ZoomHub.Types.VerificationError
  ( VerificationError (..),
  )
where

data VerificationError
  = ContentNotFound
  | TokenMismatch
