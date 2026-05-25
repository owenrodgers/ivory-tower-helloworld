module Main where

import Hello.Tests.Platforms
import Hello.Tests.BMP180 (app)

main :: IO ()
main =
  buildHelloApp
    nucleo_g474
    $ app
