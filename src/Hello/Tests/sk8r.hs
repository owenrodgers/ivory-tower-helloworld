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

{-
Organize
imuSampleTower
madgwickTower
-}
app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv
  (i2cChannel, _ready) <- i2cTower tocc platformI2C platformI2CPins
  (BackpressureTransmit imuRequest imuResponse) <- imuSampleTower i2cChannel
  redtog <- ledToggle [platformRedLED]
  ms1000 <- period (Milliseconds 10)

  (fusionInitIn, fusionInitOut) <- channel
  attitudeOut <- sensorFusion imuResponse fusionInitOut

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

    -- Periodically ask the imu for a reading
    handler ms1000 "imuRequestuester" $ do
      o <- emitter ostream 64
      sampleE <- emitter imuRequest 1

      callback $ const $ do
        -- puts o "sending request\r\n"
        t <- getTime
        emitV sampleE t

    -- IMU reading received, publish to appropriate channels
    handler imuResponse "sample_handler" $ do
      o <- emitter ostream 64
      rE <- emitter redtog 1

      callback $ \x -> do
        -- puts o "received sample\r\n"
        refCopy dbg x
        emit rE x

    -- Got an attitude update from the estimator.s
    handler attitudeOut "attitude_measurement" $ do
      o <- emitter ostream 64
      callback $ \ref -> do
        -- puts o "got an attitude\r\n"
        refCopy att ref