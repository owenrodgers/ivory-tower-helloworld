-- | I2C Magnetometer test
--
-- Periodically dumps magnetometer values
-- to UART

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.Magnetometer where

import Ivory.Language
import Ivory.Tower

import Ivory.BSP.STM32.ClockConfig (ClockConfig)
import Ivory.BSP.STM32.Driver.I2C

import Ivory.Tower.Base
import Ivory.Tower.Base.UART.Types
import Ivory.Tower.HAL.Bus.Interface (BackpressureTransmit(..))

import Ivory.Tower.Drivers.Magnetometer.HMC5883L
import Ivory.Tower.Drivers.Magnetometer.QMC5883L
import Ivory.Tower.Drivers.Magnetometer.Types
import Hello.Tests.Platforms

-- | Driver selector
--
-- HMC5883L is discontinued and recent
-- GY-273 boards have QMC5883L.
--
-- Defaults to QMC, change in test/Magnetometer.hs
data MagnetometerType
  = MagnetometerType_HMC5883L
  | MagnetometerType_QMC5883L
  deriving (Eq, Show)

-- | Dump magnetometer values in microTeslas
-- to UART along with computed heading
app :: MagnetometerType
    -> (a -> ClockConfig)
    -> (a -> Platform)
    -> Tower a ()
app magType tocc toPlatform = do
  Platform{..} <- fmap toPlatform getEnv

  (i2cTransmit, ready) <- i2cTower tocc platformI2C platformI2CPins
  togIn <- ledToggle [platformRedLED]
  per <- period (Milliseconds 1000)
  fwd per togIn

  (BackpressureTransmit magRequest magResult) <-
    case magType of
      MagnetometerType_HMC5883L ->
        hmc5883lTower
          i2cTransmit
          ready
          hmc5883DefaultAddr
          hmc5883DefaultConfig
      MagnetometerType_QMC5883L ->
        qmc5883lTower
          i2cTransmit
          ready
          qmc5883DefaultAddr
          qmc5883DefaultConfig

  ms10 <- period (Milliseconds 10)
  fwd ms10 magRequest

  uartTowerDeps
  (ostream, _istream) <-
    bufferedUartTower
      tocc
      platformUART
      platformUARTPins
      115200
      (Proxy :: Proxy UARTBuffer)

  monitor "dumpMagnetometer" $ do
    handler magResult "magResult" $ do
      o <- emitter ostream 64
      callback $ \magSample -> do
        x' <- deref (magSample ~> x)
        y' <- deref (magSample ~> y)
        z' <- deref (magSample ~> z)

        let heading = (atan2F y' x') * 180/pi

        (strX :: Ref ('Stack s) UARTBuffer) <- floatingToString x' 4
        (strY :: Ref ('Stack s) UARTBuffer) <- floatingToString y' 4
        (strZ :: Ref ('Stack s) UARTBuffer) <- floatingToString z' 4
        (strH :: Ref ('Stack s) UARTBuffer) <- floatingToString heading 4

        puts o "x "
        putIvoryString o (constRef strX)
        puts o " y "
        putIvoryString o (constRef strY)
        puts o " z "
        putIvoryString o (constRef strZ)
        puts o " heading "
        putIvoryString o (constRef strH)
        puts o "\r\n"
