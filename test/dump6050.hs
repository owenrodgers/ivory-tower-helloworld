module Main where

import Hello.Tests.Platforms
import Hello.Tests.Dump6050 (app)

main :: IO ()
main = buildHelloApp f4disco app