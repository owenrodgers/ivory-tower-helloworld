{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.Dump6050 where

    import Ivory.Language
import Ivory.Tower

import Ivory.Tower.HAL.Bus.Interface

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Base.LED (ledToggle)
import Hello.Tests.Platforms

addr :: I2CDeviceAddr
addr = I2CDeviceAddr 0x68

app :: (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app tocc toPlatform = do
    Platform{..} <- fmap toPlatform getEnv
    everySecond <- period (Milliseconds 1000)

    -- UART tower
    uartTowerDeps
    (ostream, _istream) <-
        bufferedUartTower
            tocc
            platformUART
            platformUARTPins
            115200
            (Proxy :: Proxy UARTBuffer)

    -- I2C thing
    ((BackpressureTransmit req res), _ready) <- i2cTower tocc platformI2C platformI2CPins
    togIn <- ledToggle [platformRedLED]

    monitor "i2c" $ do
        handler everySecond "i2cPer" $ do
            reqE <- emitter req 1
            callback $ const $ do
            -- whoami
            r <- local $ istruct
                    [ tx_addr   .= ival addr
                    , tx_buf    .= iarray [ival 0x75]
                    , tx_len    .= ival 1
                    , rx_len    .= ival 1
                    ]

        emit reqE (constRef r)

    dbg <- state "whoami_result"

    handler res "i2cResult" $ do
      e <- emitter togIn 1
      callback $ \x -> do
        refCopy dbg x
        emit e x
        
        -- send info to UART
        o <- emitter ostream 64
        puts o "hi\r\n"
