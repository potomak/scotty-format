{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (object, (.=))
import Data.Text.Lazy.Encoding (encodeUtf8)
import Web.Scotty (scotty, get, json, text, setHeader, raw)

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
      format "application/vnd.chess-pgn" $ do
        setHeader "Content-Type" "application/vnd.chess-pgn; charset=utf-8"
        raw $ encodeUtf8 "1. e4"

  get "/error" $
    respondTo $ return ()
