{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Parse

main :: IO ()
main = do
    test "https://user:secret@example.com:8080/dir/index.html#home"
    test "https://example.com/user/1234?email=email@example.com&age=99"
