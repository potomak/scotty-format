{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (object, (.=))
import Web.Scotty (scotty, get, json, text)

import Web.Scotty.Format.Trans (format)
import qualified Web.Scotty.Format.Trans as Format


main :: IO ()
main = scotty 8080 $ do
  get "/hello" $ do
    let content = "Hello world!"
    format $ do
      Format.json $
        json $ object ["content" .= content]
      Format.text $
        text content

  get "/error" $
    format $ return ()
