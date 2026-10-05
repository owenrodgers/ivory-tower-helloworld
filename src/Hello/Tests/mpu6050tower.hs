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

mpu6050Address :: I2CDeviceAddr
mpu6050Address = I2CDeviceAddr 0x68

accelStart :: Uint8
accelStart = 0x3B

gyroStart :: Uint8
gyroStart = 0x43

-- | Builds a tower that spits out readings from an MPU 6050
mpu6050Tower   
  :: BackpressureTransmit                -- ^ I2C channel
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
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

        forever $ do
          _read_result <- yield -- When a message is received from i2c_response the coroutine resumes here
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



{-
  Need towers for accelerometer measurements and gyroscope measurements
  Combine them into one for a full "imu" measurement
-}

type RegisterAddr = Uint8

{-
Constructs a tower that will be used for generic reads from an MPU6050
-}
readSampleTower   
  :: BackpressureTransmit                -- ^ I2C channel
       (Struct "i2c_transaction_request")
       (Struct "i2c_transaction_result")
  -> ChanOutput (Stored ITime)            -- ^ Initialization channel, send an ITime to this to initalize the device
  -> I2CDeviceAddr                        -- ^ Address of the device
  -> Tower e  
      (BackpressureTransmit               -- ^ ITime -> imu_sample
        (Stored RegisterAddr)             --   Send an address here to initiate a read from that address
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

      return $ CoroutineBody $ \yield -> do -- When a message is received from init_channel the coroutine resumes here
        comment "entry to mpu6050 coroutine"
        forever $ do
          -- send the wakeup request to the device and then break
          -- tells the mpu6050 to wake up
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

        forever $ do
          _read_result <- yield -- When a message is received from i2c_response the coroutine resumes here
                                     -- we should check if there was an i2c error
          comment "response received from periodic read"

          -- read 6 bytes
          read_request <- local $ istruct
            [ tx_addr .= ival deviceAddress
            , tx_buf  .= iarray []
            , tx_len  .= ival 0
            , rx_len  .= ival 6
            ]
          emit i2c_req_e (constRef read_request)
          res <- yield

          comment "response received from perform read request"
          store reading_in_progress false

          payloads16 res 0 1 >>= store ((current_sample ~> r) ! 0) . safeCast
          payloads16 res 2 3 >>= store ((current_sample ~> r) ! 1) . safeCast
          payloads16 res 4 5 >>= store ((current_sample ~> r) ! 2) . safeCast

          -- emit the processed sample
          emit sens_e (constRef current_sample)

    handler trigger_channel_out (named "periodic_read") $ do
      i2c_req_e <- emitter requests 1
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

  let toGs = (/ 16384.0)
  let toRads = (* ((3.1415926 / 180.0) / 131.0))

  -- initialization channel for the device
  (initIn, initOut) <- channel
  (BackpressureTransmit addrIn resultOut) <- readSampleTower (BackpressureTransmit requests results) initOut mpu6050Address

  (timeIn, timeOut) <- channel
  (samplesIn, samplesOut) <- channel
  
  monitor (named "imuSample") $ do
    sample <- state (named "mysample")

    -- start the coroutine when we get a message from timeOut
    -- resume when a sample is received
    coroutineHandler timeOut resultOut (named "sample_tower") $ do
      reqE <- emitter addrIn 1
      sampleE <- emitter samplesIn 1
      initE <- emitter initIn 1
      return $ CoroutineBody $ \yield -> do
        -- tell the mpu6050 to wake up
        t <- getTime
        emitV initE t

        -- now for the rest of time, wait for a signal and publish a sensor reading
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

  pure $ BackpressureTransmit timeIn samplesOut

  where
    named :: String -> String
    named s = s ++ "tweaker"