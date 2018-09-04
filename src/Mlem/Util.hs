-- |

module Mlem.Util where

import Mlem.Prelude
import Control.Monad.State

snapshotState :: MonadState s m => m a -> m a
snapshotState ma = do
  oldState <- get
  ret <- ma
  put oldState
  pure ret
