{-# LANGUAGE CPP #-}
module Main where

import Lib

main :: IO ()
#ifdef PRODUCT
main = noCssApp
#else
main = app
#endif
