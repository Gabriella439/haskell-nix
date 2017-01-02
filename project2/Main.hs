module Main where

foreign import ccall "check" check :: IO ()

main :: IO ()
main = check
