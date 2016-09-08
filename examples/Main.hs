{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (object, (.=))
import Web.Scotty (scotty, get, json, text)

import Web.Scotty.Format.Trans (respondTo, formatJson, formatText, format)


main :: IO ()
main = scotty 8080 $ do
  get "/hello" $ do
    let content = "Hello world!"
    respondTo $ do
      formatJson $
        json $ object ["content" .= content]
      formatText $
        text content
      format "application/vnd.chess-pgn" $
        text "1. e4"

  get "/error" $
    respondTo $ return ()
