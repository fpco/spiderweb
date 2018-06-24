{-# LANGUAGE OverloadedStrings #-}
module Main where

import SpiderWeb
import RIO

main :: IO ()
main = runSimpleApp $ download "http://localhost:3000" >>= logInfo . displayShow
