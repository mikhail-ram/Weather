module Main where

import Lib

main :: IO ()
main = do
    config' <- readFile ".config"
    let config = getConfig config'
    getWeather config
