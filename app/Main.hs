{-# LANGUAGE OverloadedStrings #-}
module Main where

import SpiderWeb

main :: IO ()
main = download "http://localhost:3000" >>= print
