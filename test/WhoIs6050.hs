module Main where

import Hello.Tests.Platforms
import Hello.Tests.WhoIs6050 (app)

main :: IO ()
main = buildHelloApp f4disco app
