{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Format.Trans (
  respondTo,
  formatHtml,
  formatText,
  formatJson,
  format,
  ResponseFormat,
) where

import Control.Monad (liftM, ap)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Lazy (toStrict)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Media (MediaType, mapAcceptMedia)
import Network.HTTP.Types (notAcceptable406)
import Web.Scotty.Trans (ActionT, ScottyError, status, header)


respondTo :: (ScottyError e, Monad m)
  => ResponseFormat e m ()
  -> ActionT e m ()
respondTo (RF preferences ()) = do
  accept <- maybe "*/*" (encodeUtf8 . toStrict) <$> header "Accept"
  fromMaybe (status notAcceptable406) (mapAcceptMedia preferences accept)


formatHtml :: (ScottyError e, Monad m)
  => ActionT e m ()
  -> ResponseFormat e m ()
formatHtml = format "text/html"


formatText :: (ScottyError e, Monad m)
  => ActionT e m ()
  -> ResponseFormat e m ()
formatText = format "text/plain"


formatJson :: (ScottyError e, Monad m)
  => ActionT e m ()
  -> ResponseFormat e m ()
formatJson = format "application/json"


format :: (ScottyError e, Monad m)
  => MediaType
  -> ActionT e m ()
  -> ResponseFormat e m ()
format mediaType action = RF [(mediaType, action)] ()


data ResponseFormat e m a = RF [(MediaType, ActionT e m ())] a

-- Private

instance Monad (ResponseFormat e m) where
  return = RF []
  RF formats a >>= f =
    let RF newFormats b = f a
    in RF (formats <> newFormats) b

instance Functor (ResponseFormat e m) where
  fmap = liftM

instance Applicative (ResponseFormat e m) where
  pure = return
  (<*>) = ap

instance Monoid (ResponseFormat e m a) where
  mempty = RF mempty undefined
  mappend (RF a _) (RF b _) = RF (a <> b) undefined
