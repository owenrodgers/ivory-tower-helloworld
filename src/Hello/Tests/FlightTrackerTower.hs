{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hello.Tests.FlightTrackerTower where

import Ivory.Language
import Ivory.Tower  

import Hello.Tests.Mpu6050Tower (az)

[ivory|
 struct orientation_delta
   { droll          :: Stored IFloat
   ; dpitch         :: Stored IFloat
   ; dyaw           :: Stored IFloat
   }
|]

trackerTypes :: Module
trackerTypes = package "flightTrackerTypes" $ do
  defStruct (Proxy :: Proxy "orientation_delta")

flightTrackerTower
    :: ChanOutput (Stored ITime)
    -> ChanOutput (Struct "imu_sample")
    -> Tower e
        (ChanOutput (Struct "orientation_delta"))

flightTrackerTower trackInit samplesOut = do
    towerModule trackerTypes
    towerDepends trackerTypes
    (deltas_in, deltas_out) <- channel

    monitor (named "flightmon") $ do
        in_flight <- stateInit (named "in_flight") (ival false)
        tracked_delta <- state (named "tracked_ori_delta")
        {-
        orientation <- state (named "ori")
        -}

        handler trackInit (named "watchforinit") $ do
            die <- emitter deltas_in 1
            callback $ const $ do
                flying <- deref in_flight
                ifte_ flying
                    (do 
                        -- we just landed, publish orientation delta
                        emit die (constRef tracked_delta)
                        )
                    
                    (do
                        -- now we're in flight
                        pure ()
                        )
                store in_flight (iNot flying)
        
        handler samplesOut (named "samplesWatcher") $ do
            {-
            se <- emitter samples_in
            -}
            callback $ \s -> do
                {-
                emit se imu_sample

                In a callback for orientations from madgwick
                -}

                accz <- deref (s ~> az)
                delta <- local $ istruct
                    [   droll .= ival 1.0
                    ,   dpitch .= ival accz
                    ,   dyaw .= ival 3.0
                    ]

                refCopy tracked_delta delta
            
    pure deltas_out
    
    where
        named :: String -> String
        named nm = "_tracka" ++ nm