-- | I2C Temperature / pressure readout test from BMP180
--
-- Periodically dumps values to UART

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.BMP180 where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types

import Ivory.Tower.Drivers.Pressure.BMP180
import Hello.Tests.Platforms

app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (i2cTransmit, ready) <- i2cTower tocc platformI2C platformI2CPins
  togIn <- ledToggle [platformRedLED]
  per <- period (Milliseconds 1000)
  fwd per togIn

  bmpResult <-
     bmp180Tower
       i2cTransmit
       ready
       Oversample8

  uartTowerDeps
  (ostream, _istream) <-
    bufferedUartTower
      tocc
      platformUART
      platformUARTPins
      115200
      (Proxy :: Proxy UARTBuffer)

  monitor "dumpBMP" $ do
    handler bmpResult "bmpResult" $ do
      o <- emitter ostream 64
      callback $ \bmpSample -> do
        t <- deref (bmpSample ~> bmp_sample_temperature)
        p <- deref (bmpSample ~> bmp_sample_pressure)

        let
          a = pressureToAltitude
                defaultPressureAtSeaLevel
                p

        (strT :: Ref ('Stack s) UARTBuffer) <- floatingToString t 4
        (strP :: Ref ('Stack s) UARTBuffer) <- floatingToString p 4
        (strA :: Ref ('Stack s) UARTBuffer) <- floatingToString a 4

        puts o "T "
        putIvoryString o (constRef strT)
        puts o " P "
        putIvoryString o (constRef strP)
        puts o " Alt "
        putIvoryString o (constRef strA)
        puts o "\r\n"
