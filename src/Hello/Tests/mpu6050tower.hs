{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hello.Tests.Mpu6050Tower where

import Ivory.Language
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.BSP.STM32.Driver.I2C

-- struct to represent a nice reading from the mpu6050
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

-- struct for three raw int16's from the mpu6050
[ivory|
  struct read_result
    { r :: Array 3 (Stored Sint16) }
|]

mpu6050Types :: Module
mpu6050Types = package "mpu6050Types" $ do
  defStruct (Proxy :: Proxy "imu_sample")
  defStruct (Proxy :: Proxy "read_result")

mpu6050Address :: I2CDeviceAddr
mpu6050Address = I2CDeviceAddr 0x68

type RegisterAddr = Uint8

pwrMgmt :: RegisterAddr
pwrMgmt = 0x6B

accelStart :: RegisterAddr
accelStart = 0x3B

gyroStart :: RegisterAddr
gyroStart = 0x43

{-
Constructs a channel that can be used to read the accelerometer and gyroscope values
from an MPU 6050.
-}
imuSampleTower   
  :: BackpressureTransmit
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> Tower e  
      (BackpressureTransmit
        (Stored ITime)
        (Struct "imu_sample")
      )
imuSampleTower (BackpressureTransmit requests results) = do
  towerModule mpu6050Types
  towerDepends mpu6050Types

  -- Scalings for the default sensitivites of the mpu6050
  let toGs = (/ 16384.0)
  let toRads = (* ((3.1415926 / 180.0) / 131.0))

  -- initialization channel for the mpu 6050
  (initIn, initOut) <- channel
  (BackpressureTransmit addrIn resultOut) <- readSampleTower (BackpressureTransmit requests results) initOut mpu6050Address

  -- channel used to signal the tower to start
  (timeIn, timeOut) <- channel

  -- channel used to emit samples from the imu
  (samplesIn, samplesOut) <- channel
  
  monitor (named "imuSample") $ do
    sample <- state (named "mysample")

    -- start the coroutine when we get an initialization message from timeOut and then resume
    -- the coroutine any time a sample from the sensor is received. This hides the device
    -- initialization details from the caller (me).
    coroutineHandler timeOut resultOut (named "sample_tower") $ do
      reqE <- emitter addrIn 1
      sampleE <- emitter samplesIn 1
      initE <- emitter initIn 1
      return $ CoroutineBody $ \yield -> do
        -- tell the mpu6050 to wake up
        t <- getTime
        emitV initE t

        -- now for the rest of time, wait for a signal and then publish a sensor reading
        forever $ do
          _r <- yield

          -- fire off our requests for accelerometer and gyro data
          let rpc req = emitV reqE req >> yield
          res1 <- rpc accelStart
          res2 <- rpc gyroStart
            
          -- store and emit
          store (sample ~> ax) . toGs . safeCast =<< deref ((res1 ~> r) ! 0)
          store (sample ~> ay) . toGs . safeCast =<< deref ((res1 ~> r) ! 1)
          store (sample ~> az) . toGs . safeCast =<< deref ((res1 ~> r) ! 2)

          store (sample ~> gx) . toRads . safeCast =<< deref ((res2 ~> r) ! 0)
          store (sample ~> gy) . toRads . safeCast =<< deref ((res2 ~> r) ! 1)
          store (sample ~> gz) . toRads . safeCast =<< deref ((res2 ~> r) ! 2)

          emit sampleE (constRef sample)

  -- the caller gets a nice channel that accepts ITime's and outputs sensor readings.
  pure $ 
    BackpressureTransmit 
      timeIn 
      samplesOut

  where
    named :: String -> String
    named s = s ++ "tweaker"

{-
Constructs a tower that performs generic reads from an MPU6050
-}
readSampleTower   
  :: BackpressureTransmit                 -- ^ I2C channel
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> ChanOutput (Stored ITime)            -- ^ Initialization channel, send an ITime to this to initalize the device
  -> I2CDeviceAddr                        -- ^ Address of the device
  -> Tower e  
      (BackpressureTransmit               -- ^ Register Address -> IMU Sample
        (Stored RegisterAddr)             --   Send an address here to initiate a read beginning at that address
        (Struct "read_result")
      )
readSampleTower (BackpressureTransmit requests results) initChannel deviceAddress = do
  towerModule mpu6050Types
  towerDepends mpu6050Types

  (sensor_channel_in, sensor_channel_out) <- channel
  (trigger_channel_in, trigger_channel_out) <- channel

  monitor (named "SensorManager") $ do
    -- device is awake and ready to be read from
    initialized         <- stateInit (named "initialized")      (ival false)
    -- reading in progress
    reading_in_progress <- stateInit (named "read_in_progress") (ival false)
    -- current sample
    current_sample      <- state     (named "current_sample")

    coroutineHandler initChannel results (named "coroutine") $ do
      -- send i2c requests with this emitter
      i2c_req_e <- emitter requests 1

      -- we emit our samples here, the 'out' end is returned
      sens_e <- emitter sensor_channel_in 1

      -- this coroutine starts when a message is received from initChannel.
      return $ CoroutineBody $ \yield -> do

        -- This part of the coroutine initializes the device if it is not already awake.
        -- This requires sending a 0 to the power management register (pwrMgmt)
        forever $ do
          -- send the wakeup request
          wakeup_request <- local $ istruct
            [ tx_addr .= ival deviceAddress
            , tx_buf  .= iarray [ival pwrMgmt, ival 0]
            , tx_len  .= ival 2
            , rx_len  .= ival 0
            ]
          -- TODO: handle an i2c failure
          emit i2c_req_e (constRef wakeup_request)
          breakOut

        -- we're good to go... probably
        store initialized true

        -- This part of the coroutine handles the response to a read request to the sensor.
        -- When a response is received the raw values are packed into a struct of 3 int16's. This
        -- This way scaling and conversion is offloaded to the caller.
        forever $ do
          _read_result <- yield -- When a message is received from i2c_response the coroutine resumes here
                                -- we should check if there was an i2c error

          -- Send our request for 6 bytes and wait for a response.
          read_request <- local $ istruct
            [ tx_addr .= ival deviceAddress
            , tx_buf  .= iarray []
            , tx_len  .= ival 0
            , rx_len  .= ival 6
            ]
          emit i2c_req_e (constRef read_request)
          res <- yield

          -- Pack the bytes into a struct and emit the sample.
          store reading_in_progress false

          payloads16 res 0 1 >>= store ((current_sample ~> r) ! 0) . safeCast
          payloads16 res 2 3 >>= store ((current_sample ~> r) ! 1) . safeCast
          payloads16 res 4 5 >>= store ((current_sample ~> r) ! 2) . safeCast

          -- emit the processed sample
          emit sens_e (constRef current_sample)

    -- This handler responds to read requests from the caller.
    handler trigger_channel_out (named "periodic_read") $ do
      i2c_req_e <- emitter requests 1

      -- Got an address to send our request to, update sensor state and fire only when
      -- the device is awake and another read isn't in progress.
      callback $ \txAddress -> do
        is_initialized <- deref initialized
        is_in_progress <- deref reading_in_progress
        txTo <- deref txAddress

        ifte_ (is_initialized .&& iNot is_in_progress) 
          ( do
          -- send a read request
          read_request <- local $ istruct
            [ tx_addr .= ival deviceAddress
            , tx_buf  .= iarray [ ival txTo ]
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
  named nm = "generic_read" ++ nm