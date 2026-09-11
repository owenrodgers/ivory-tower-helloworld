{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hello.Tests.Sk8r where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Hello.Tests.Platforms
import Ivory.BSP.STM32.Driver.I2C
import Ivory.Tower.HAL.Bus.Interface
import Hello.Tests.Mpu6050Tower

app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv
  (init_in, init_out) <- channel
  (i2c_channel, _ready) <- i2cTower tocc platformI2C platformI2CPins
  (BackpressureTransmit mpu_req mpu_res) <- mpu6050Tower i2c_channel init_out addr
  ms1000 <- period (Milliseconds 1000)

  uartTowerDeps
  (ostream, _istream) <-
    bufferedUartTower
      tocc
      platformUART
      platformUARTPins
      115200
      (Proxy :: Proxy UARTBuffer)

  monitor "myMonitor" $ do
    dbg <- state "sample_result"
    mpu6050init <- stateInit "mpu6050_initialized" (ival false)

    handler ms1000 "tick" $ do
      o <- emitter ostream 64
      sample_emitter <- emitter mpu_req 1
      init_emitter <- emitter init_in 1

      callback $ const $ do
        ready <- deref mpu6050init
        ifte_ ready
          (do 
              puts o "sending request\r\n"
              t <- getTime
              emitV sample_emitter t
            )
          (do
            puts o "initializing device\r\n"
            t <- getTime
            emitV init_emitter t
            store mpu6050init true
            puts o "device initialized\r\n"
          )

    handler mpu_res "sample_handler" $ do
      o <- emitter ostream 64
      callback $ \x -> do
        puts o "we got a response\r\n"
        refCopy dbg x