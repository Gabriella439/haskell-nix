{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (empty)
import qualified Project3
import qualified Turtle

main :: IO ()
main = do
    Turtle.touch "example"
    Turtle.procs "bsdtar" ["cf", "example.tar", "example"] empty
    Project3.check
