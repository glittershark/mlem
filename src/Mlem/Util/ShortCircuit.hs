-- |

module Mlem.Util.ShortCircuit
  ( ReturnT
  , return
  , runReturn
  ) where

import Mlem.Prelude
import Data.Void
import Control.Monad.Except hiding (return)
import Control.Monad.State hiding (return)

newtype ReturnT m r a = ReturnT { unReturnT :: ExceptT r m a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadState s
                   )

return :: Monad m => r -> ReturnT m r a
return = ReturnT . throwError

runReturn :: Functor m => ReturnT m a a -> m a
runReturn = fmap (either id id) . runExceptT . unReturnT

runReturn' :: Functor m => ReturnT m a Void -> m a
runReturn' = fmap (either id absurd) . runExceptT . unReturnT
