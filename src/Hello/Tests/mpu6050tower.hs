{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Hello.Tests.Mpu6050Tower
  ( mpu6050HelloTower
  , mpu6050DefaultAddr
  ) where

import Ivory.Language
import Ivory.Stdlib
import Ivory.Tower
import Ivory.Tower.HAL.Bus.Interface
import Ivory.Tower.HAL.Bus.I2C.DeviceAddr (I2CDeviceAddr(..))

--
 --- MPU6050 Constants ---
-- define MPU6050_I2C_ADDR 0x68
-- https://github.com/ayushgaud/MPU6050/blob/master/MPU6050.h
-- REG_PWR_MGMT_1   0x6B
-- REG_ACCEL_X_OUT  0x3B
-- REG_ACCEL_Y_OUT  0x3D
-- REG_ACCEL_Z_OUT  0x3F
-- REG_GYRO_X_OUT   0x43
-- REG_GYRO_Y_OUT   0x45
-- REG_GYRO_Z_OUT   0x47

-- The default address of the GY-521 breakout is 0x68
mpu6050DefaultAddr :: I2CDeviceAddr
mpu6050DefaultAddr = I2CDeviceAddr 0x68

-- Power managements and whoami registers
regPwrMgmt1, regWhoAmI :: Uint8
regPwrMgmt1 = 0x6B
regWhoAmI   = 0x75

mpu6050HelloTower
  :: BackpressureTransmit
       ('Struct "i2c_transaction_request")
       ('Struct "i2c_transaction_result")
  -> ChanOutput ('Stored ITime) -- 
  -> I2CDeviceAddr
  -> Tower e (ChanOutput ('Stored Uint8))

