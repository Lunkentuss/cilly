module ExceptionWrapper (
  wrapException
) where

import Control.Exception (
    bracket
  , catches
  , Handler(..)
  , SomeException
  , SomeAsyncException(..)
  , Exception
  , throw)

data WrappedException e = WrappedException String e

instance Show e => Show (WrappedException e) where
  show (WrappedException ctx e) = ctx <> ": " <> show e

instance (Exception e) => Exception (WrappedException e)

wrapException :: IO a -> String -> IO a
wrapException action ctx =
  catches
  action
  [
      Handler $ \(e :: SomeException) -> throw $ WrappedException ctx e
    , Handler $ \(e :: SomeAsyncException) -> throw e
  ]
