{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parse

main :: IO ()
main = test "https://user:secret@example.com:8080/index.html#home"
