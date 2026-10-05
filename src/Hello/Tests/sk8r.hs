{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hello.Tests.Sk8r where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.Base
import Ivory.Tower.Base.LED (ledToggle)
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.Driver.I2C
import Ivory.BSP.STM32.ClockConfig (ClockConfig)

import Hello.Tests.Platforms
import Hello.Tests.Mpu6050Tower
import Hello.Tests.FlightTrackerTower
import Hello.Tests.MadgwickTower

app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv
  (i2c_channel, _ready) <- i2cTower tocc platformI2C platformI2CPins
  (BackpressureTransmit mpu_req mpu_res) <- imuSampleTower i2c_channel
  redtog <- ledToggle [platformRedLED]
  ms1000 <- period (Milliseconds 10)

  (fusionInitIn, fusionInitOut) <- channel
  attitudeOut <- sensorFusion mpu_res fusionInitOut

  uartTowerDeps
  (ostream, _istream) <-
    bufferedUartTower
      tocc
      platformUART
      platformUARTPins
      115200
      (Proxy :: Proxy UARTBuffer)

  monitor "sk8r_monitor" $ do
    dbg <- state "sample_result"
    att <- state "current_attitude"

    -- Ask the imu for a reading
    handler ms1000 "mpu_requester" $ do
      o <- emitter ostream 64
      sample_emitter <- emitter mpu_req 1

      callback $ const $ do
        -- puts o "sending request\r\n"
        t <- getTime
        emitV sample_emitter t

    -- we got an imu reading
    handler mpu_res "sample_handler" $ do
      o <- emitter ostream 64
      re <- emitter redtog 1

      callback $ \x -> do
        -- puts o "received sample\r\n"
        refCopy dbg x
        emit re x

    handler attitudeOut "attitude_measurement" $ do
      o <- emitter ostream 64
      callback $ \ref -> do
        -- puts o "got an attitude\r\n"
        refCopy att ref