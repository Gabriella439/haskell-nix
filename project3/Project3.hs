{-# LANGUAGE ForeignFunctionInterface #-}

module Project3 where

foreign import ccall "check" check :: IO ()
