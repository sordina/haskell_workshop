{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = scotty 3030 $ do
  get "/" $ do
    liftIO ( putStrLn "User requested '/' resource..." )
    text "lol"
