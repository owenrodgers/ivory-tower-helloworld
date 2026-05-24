module Main where

import Hello.Tests.Platforms
import Hello.Tests.Magnetometer (app, MagnetometerType(..))

main :: IO ()
main =
  buildHelloApp
    nucleo_g474
    $ app
        MagnetometerType_QMC5883L
