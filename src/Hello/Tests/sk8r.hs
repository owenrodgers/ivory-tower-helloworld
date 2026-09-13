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

{-
Maybe use DMP black magic but there's zero documentation

State machine

in-flight
  start message to madgwick
  send imu data to madgwick

landed
  stop message to madgwick
  await orientation delta
    orientation delta -> score
    light up some leds or something

MadgwickTower ::
  Channel
    flight-start
    flight-end

  ->
  ChanOutput
    imu-reading

  ->
  ChanOutput
    orientation-delta

  flight-start message
    toggle led
    nuke the filter
    start tracking
    
    callback for imu-reading
      v <- reading
      q <- madgwick orientation
      q_prev <- previous orientation
      

      dq <- q^-1 * q_prev
      eulers from dq, gives droll, dpitch, dyaw

  flight-end message
    emit (droll, dpitch, dyaw)
-}

app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv
  (init_in, init_out) <- channel
  (i2c_channel, _ready) <- i2cTower tocc platformI2C platformI2CPins
  (BackpressureTransmit mpu_req mpu_res) <- mpu6050Tower i2c_channel init_out addr
  redtog <- ledToggle [platformRedLED]
  greentog <- ledToggle [platformGreenLED]
  ms1000 <- period (Milliseconds 100)

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
    in_flight <- stateInit "in_flight" (ival false)

    handler ms1000 "tick" $ do
      o <- emitter ostream 64
      sample_emitter <- emitter mpu_req 1
      init_emitter <- emitter init_in 1

      callback $ const $ do
        ready <- deref mpu6050init
        ifte_ ready
          (do 
              --puts o "sending request\r\n"
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
      re <- emitter redtog 1
      ge <- emitter greentog 1
      callback $ \x -> do
        refCopy dbg x
        is_in_flight <- deref in_flight
        -- puts o "received response\r\n"
        emit re x

        accz <- deref (x ~> az)
        ifte_ (accz >? 1.5 .&& iNot is_in_flight)
          (do 
            store in_flight true
            emit ge x
            puts o "in flight\r\n"
            )
          (do pure ())

        ifte_ (accz <? 0.0 .&& is_in_flight)
          (do
            store in_flight false
            emit ge x
            puts o "landed\r\n" 
            )
          (do pure ())