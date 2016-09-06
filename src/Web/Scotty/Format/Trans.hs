{-# LANGUAGE OverloadedStrings #-}

module Web.Scotty.Format.Trans (
  format,
  html,
  text,
  json
) where

import Control.Monad (liftM, ap)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Text.Lazy (Text, toLower)
import Network.HTTP.Types (notAcceptable406)
import Web.Scotty.Trans (ActionT, ScottyError, status, header)


format :: (ScottyError e, Monad m) => ResponseFormat e m () -> ActionT e m ()
format (RF [] _) =
  status notAcceptable406
format (RF allFormats@((_, defaultAction) : _) ()) = do
    accept <- fmap toLower <$> header "Accept"
    maybe defaultAction (fromMaybe defaultAction . lookupFormat) accept
  where
    lookupFormat = (`lookup` allFormats)


html :: (ScottyError e, Monad m) => ActionT e m () -> ResponseFormat e m ()
html action = RF [("text/html", action)] ()


text :: (ScottyError e, Monad m) => ActionT e m () -> ResponseFormat e m ()
text action = RF [("text/plain", action)] ()


json :: (ScottyError e, Monad m) => ActionT e m () -> ResponseFormat e m ()
json action = RF [("application/json", action)] ()


-- Private

type Format = Text

data ResponseFormat e m a = RF [(Format, ActionT e m ())] a

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
