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

[ivory|
 struct imu_sample
   { ax          :: Stored IFloat
   ; ay          :: Stored IFloat
   ; az          :: Stored IFloat
   ; gx          :: Stored IFloat
   ; gy          :: Stored IFloat
   ; gz          :: Stored IFloat
   }
|]

mpu6050Types :: Module
mpu6050Types = package "mpu6050Types" $ do
  defStruct (Proxy :: Proxy "imu_sample")

addr :: I2CDeviceAddr
addr = I2CDeviceAddr 0x68

pwrMgmt, gX, gY, gZ, aX, aY, aZ :: Uint8
pwrMgmt = 0x6B
aX = 0x3B
aY = 0x3D
aZ = 0x3F
gX = 0x43
gY = 0x45
gZ = 0x47

-- | Builds a tower that spits out readings from an MPU 6050
mpu6050Tower   
  :: (BackpressureTransmit                -- ^ I2C channel
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  )
  -> ChanOutput (Stored ITime)            -- ^ Initialization channel, send an ITime to this to initalize the device
  -> I2CDeviceAddr                        -- ^ Address of the device
  -> Tower e  
      (BackpressureTransmit               -- ^ ITime -> imu_sample
        (Stored ITime)                    --   Send an ITime here to initiate a read
        (Struct "imu_sample")
      )
mpu6050Tower (BackpressureTransmit i2c_request i2c_response) init_channel device_addr = do
  towerModule mpu6050Types  -- we need the definition of the imu sample struct to be compiled into C
  towerDepends mpu6050Types

  let
    toGs = (/ 16384.0)

  -- we're returning (trigger_channel_in, sensor_channel_out)
  -- so we listen for events from trigger_channel_out
  -- and emit readings to sensor_channel_in
  (sensor_channel_in, sensor_channel_out) <- channel
  (trigger_channel_in, trigger_channel_out) <- channel

  monitor (named "SensorManager") $ do
    -- device is awake and ready to be read from
    initialized         <- stateInit (named "initialized")      (ival false)
    -- reading in progress
    reading_in_progress <- stateInit (named "read_in_progress") (ival false)
    -- current sample
    current_sample      <- state     (named "current_sample")

    coroutineHandler init_channel i2c_response (named "coroutine") $ do
      -- send i2c requests with this emitter
      i2c_req_e <- emitter i2c_request 1

      -- we emit our samples here, the 'out' end is returned
      sens_e <- emitter sensor_channel_in 1

      return $ CoroutineBody $ \yield -> do -- When a message is received from init_channel the coroutine resumes here
        comment "entry to mpu6050 coroutine"
        forever $ do
          -- send the wakeup request to the device and then break
          -- tells the mpu6050 to wake up
          wakeup_request <- local $ istruct
            [ tx_addr .= ival addr
            , tx_buf  .= iarray [ival pwrMgmt, ival 0]
            , tx_len  .= ival 2
            , rx_len  .= ival 0
            ]
          -- TODO: handle an i2c failure
          emit i2c_req_e (constRef wakeup_request)
          breakOut

        -- we're good to go... probably
        store initialized true
        comment "finished initializing in hmc5883l coroutine"

        forever $ do
          setup_read_result <- yield -- When a message is received from i2c_response the coroutine resumes here
                                     -- we should check if there was an i2c error
          comment "response received from periodic read"

          -- read 6 bytes
          read_request <- local $ istruct
            [ tx_addr .= ival device_addr
            , tx_buf  .= iarray []
            , tx_len  .= ival 0
            , rx_len  .= ival 6
            ]
          emit i2c_req_e (constRef read_request)
          res <- yield

          comment "response received from perform read request"
          store reading_in_progress false

          payloads16 res 0 1 >>= store (current_sample ~> ax) . toGs . safeCast
          payloads16 res 2 3 >>= store (current_sample ~> ay) . toGs . safeCast
          payloads16 res 4 5 >>= store (current_sample ~> az) . toGs . safeCast

          -- emit the processed sample
          emit sens_e (constRef current_sample)

    handler trigger_channel_out (named "periodic_read") $ do
      i2c_req_e <- emitter i2c_request 1
      callback $ const $ do
        is_initialized <- deref initialized
        is_in_progress <- deref reading_in_progress

        ifte_ (is_initialized .&& iNot is_in_progress) 
          ( do
          -- send a read request
          read_request <- local $ istruct
            [ tx_addr .= ival device_addr
            , tx_buf  .= iarray [ ival aX ]
            , tx_len  .= ival 1
            , rx_len  .= ival 0
            ]
          store reading_in_progress true
          emit i2c_req_e (constRef read_request))

          (do pure ())

  pure
    $ BackpressureTransmit
        trigger_channel_in
        sensor_channel_out
  where
  payloads16
    :: Ref s ('Struct "i2c_transaction_result")
    -> Ix 128
    -> Ix 128
    -> Ivory eff Sint16
  payloads16 res ixhi ixlo = do
    hi <- deref ((res ~> rx_buf) ! ixhi)
    lo <- deref ((res ~> rx_buf) ! ixlo)
    assign $ twosComplementCast ((safeCast hi `iShiftL` 8) .| safeCast lo)

  named :: String -> String
  named nm = "mpu6050_" ++ nm

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