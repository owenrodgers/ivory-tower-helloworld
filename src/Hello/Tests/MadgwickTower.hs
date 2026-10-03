{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Hello.Tests.MadgwickTower where

import Ivory.Language
import Ivory.Tower  
import Hello.Tests.FlightTrackerTower (droll, dpitch, dyaw)
import Hello.Tests.Mpu6050Tower (ax, ay, az, gx, gy, gz)

import Control.Monad (forM)

-- Vector 3
data V3 a = V3 a a a

instance Functor V3 where
  fmap g (V3 a1 b1 c1) = V3 (g a1) (g b1) (g c1) 

instance Applicative V3 where
  pure x = V3 x x x
  (V3 a b c) <*> (V3 d e f) = V3 (a d) (b e) (c f)

-- ... interesting, thanks linear
-- https://hackage-content.haskell.org/package/linear-1.23.3/docs/src/Linear.V3.html#V3
instance Monad V3 where
  V3 a b c >>= f = V3 a' b' c' where
    V3 a' _ _ = f a
    V3 _ b' _ = f b
    V3 _ _ c' = f c

instance (Num a) => Num (V3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs (V3 a b c) = V3 (abs a) (abs b) (abs c)
  signum = fmap signum
  fromInteger = pure . fromInteger
  negate = fmap negate

dotter :: (Num a) => V3 a -> V3 a -> a
dotter (V3 a1 b1 c1) (V3 a2 b2 c2) = (a1 * a2) + (b1 * b2) + (c1 * c2)

crosser :: (Num a) => V3 a -> V3 a -> V3 a
crosser (V3 a1 b1 c1) (V3 a2 b2 c2) = 
    V3 (b1*c2 - c1*b2) (c1*a2 - a1*c2) (a1*b2 - b1*a2)

normalizeV3 :: V3 IFloat -> V3 IFloat
normalizeV3 (V3 a b c) = V3 (a * inv) (b * inv) (c * inv)
  where
    n   = sqrt (a * a + b * b + c * c)
    inv = (n >? 0) ? (1 / n, 0)

infixl 7 *^
(*^) :: (Num a) => a -> V3 a -> V3 a
(*^) s (V3 a b c) = V3 (s * a) (s * b) (s * c)

-- Quaternion
data Quaternion a = Quaternion a (V3 a)

pureQuat :: Num a => V3 a -> Quaternion a
pureQuat v = Quaternion 0 v

instance Functor Quaternion where
  fmap f (Quaternion a v)= Quaternion (f a) (fmap f v)

instance Applicative Quaternion where
  pure x = Quaternion x (pure x)
  (Quaternion a v1) <*> (Quaternion e v2)  = Quaternion (a e) (v1 <*> v2) 

instance Monad Quaternion where
  Quaternion a (V3 b c d) >>= f = Quaternion a' (V3 b' c' d') where
    Quaternion a' _          = f a
    Quaternion _ (V3 b' _ _) = f b
    Quaternion _ (V3 _ c' _) = f c
    Quaternion _ (V3 _ _ d') = f d

instance (Num a) => Num (Quaternion a) where
  (+) = liftA2 (+)
  (Quaternion s1 v1) * (Quaternion s2 v2) =
    Quaternion (s1 * s2 - dotter v1 v2) (crosser v1 v2 + s1 *^ v2 + s2 *^ v1)

  abs (Quaternion a v)= Quaternion (abs a) (abs v) 
  signum (Quaternion a v)= Quaternion (signum a) (signum v) 
  fromInteger = pure . fromInteger
  negate (Quaternion a v)= Quaternion (negate a) (negate v) 

normalizeQuat :: Quaternion IFloat -> Quaternion IFloat
normalizeQuat (Quaternion w (V3 i j k)) = Quaternion (w * inv) (V3 (i * inv) (j * inv) (k * inv))
  where
    n   = sqrt (w * w + i * i + j * j + k * k)
    inv = (n >? 0) ? (1 / n, 0)

infixl 7 ~*^
(~*^) :: (Num a) => a -> Quaternion a -> Quaternion a
(~*^) s (Quaternion a v) = Quaternion (s * a) (s *^ v)

-- they're isomorphic as a real vector space so this is fine.
type Matrix4x3 a = Quaternion (V3 a)

-- running out of ideas for operator names
infixl 7 ^*^
(^*^) :: (Num a) => Matrix4x3 a -> V3 a -> Quaternion a
(^*^) m v = fmap (dotter v) m -- curry technology

derefQuat :: Ref s ('Array 4 ('Stored IFloat)) -> Ivory eff (Quaternion IFloat)
derefQuat ref = do
  i <- deref (ref ! 0)
  x <- deref (ref ! 1)
  y <- deref (ref ! 2)
  z <- deref (ref ! 3)
  return (Quaternion i (V3 x y z))

storeQuat
  :: Ref s ('Array 4 ('Stored IFloat)) -> Quaternion IFloat -> Ivory eff ()
storeQuat ref (Quaternion i (V3 x y z)) = do
  store (ref ! 0) i
  store (ref ! 1) x
  store (ref ! 2) y
  store (ref ! 3) z

-- from https://github.com/owenrodgers/Madgwick/blob/master/src/madgwick.rs
[ivory| 
    struct madgwick_imu
        {   delta_t :: Stored IFloat
        ;   beta :: Stored IFloat
        ;   orientation :: Array 4 (Stored IFloat)
        }
|]

data MadgwickState = MadgwickState {
      deltaTime :: IFloat
    , betaCoefficient :: IFloat
    , currentOrientation :: Quaternion IFloat
}

derefMadgwickState :: Ref s ('Struct "madgwick_imu") -> Ivory eff MadgwickState
derefMadgwickState state = do
    deltaTime <- deref (state ~> delta_t)
    betaCoefficient <- deref (state ~> beta)
    currentOrientation <- derefQuat (state ~> orientation)
    pure MadgwickState {..}

storeMadgwickState :: Ref s ('Struct "madgwick_imu") -> MadgwickState -> Ivory eff ()
storeMadgwickState stateRef MadgwickState {..} = do
    store (stateRef ~> delta_t) deltaTime
    store (stateRef ~> beta) betaCoefficient
    storeQuat (stateRef ~> orientation) currentOrientation

derefImuSample :: ConstRef s ('Struct "imu_sample") -> Ivory eff (V3 IFloat, V3 IFloat)
derefImuSample sampleRef = do
    ax' <- deref (sampleRef ~> ax)
    ay' <- deref (sampleRef ~> ay)
    az' <- deref (sampleRef ~> az)
    gx' <- deref (sampleRef ~> gx)
    gy' <- deref (sampleRef ~> gy)
    gz' <- deref (sampleRef ~> gz)
    pure ( V3 ax' ay' az', V3 gx' gy' gz' )

{-
-- Pattern stolen from:
-- https://github.com/GaloisInc/smaccmpilot-stm32f4/blob/1ba4a81c322003365166ea65c39576c67f6560f3/src/smaccm-flight/src/SMACCMPilot/Flight/Control/Attitude/KalmanFilter.hs#L509
-}
data AttEstimator = 
    AttEstimator
        {   madgwick_init :: forall eff . Ivory eff ()
        ,   madgwick_update
                :: forall eff s1
                 . ConstRef s1 ('Struct "imu_sample")
                -> Ivory eff ()
        ,   madgwick_state :: Ref 'Global ('Struct "madgwick_imu")
        }

{-
Initializes the madgwick filter
-}
madgwickInit :: Ref s ('Struct "madgwick_imu") -> Def ('[] ':-> ())
madgwickInit stateRef = voidProc (named "init") $ body $ do
    let filter_init = MadgwickState {
          deltaTime = 0.01
        , betaCoefficient = 0.1
        , currentOrientation = quatRotId
    }

    storeMadgwickState stateRef filter_init
    pure ()

quatRotId :: Quaternion IFloat
quatRotId = Quaternion 1.0 (V3 0.0 0.0 0.0)

{-
Given a sample from the imu, update the madgwick state
Implementation from: https://github.com/owenrodgers/Madgwick/blob/master/src/madgwick.rs#L133
-}
madgwickUpdate :: Ref s1 ('Struct "madgwick_imu") -> Def ('[ConstRef s2 ('Struct "imu_sample")] ':-> ())
madgwickUpdate stateRef = 
    voidProc (named "madgwick_update") $ \sample -> body $ do
        MadgwickState { .. } <- derefMadgwickState stateRef -- all members are brought into scope
        (acc, gyro) <- derefImuSample sample
        f_g <- buildObjectiveFunction currentOrientation acc
        jacobian_g <- buildJacobian currentOrientation
        let grad = normalizeQuat (jacobian_g ^*^ f_g)
        let q_w = currentOrientation * (0.5 ~*^ pureQuat gyro)
        let q_est_dot = q_w - (betaCoefficient ~*^ grad)
        let newOrientation = normalizeQuat (currentOrientation + (deltaTime ~*^ q_est_dot))

        storeQuat (stateRef ~> orientation) newOrientation

buildJacobian :: Quaternion IFloat -> Ivory curryTechnology (Matrix4x3 IFloat)
buildJacobian (Quaternion w (V3 i j k)) = do
  pure (Quaternion
        (V3 (-(2.0 * j)) (2.0 * i) 0.0)
        (V3
          (V3 (2.0 * k) (2.0 * w) (-(4.0 * i)))
          (V3 (-(2.0 * w)) (2.0 * k) (-(4.0 * j)))
          (V3 (2.0 * i) (2.0 * j) 0.0)
        )
      )

buildObjectiveFunction :: Quaternion IFloat -> V3 IFloat -> Ivory eff (V3 IFloat)
buildObjectiveFunction (Quaternion w (V3 i j k)) acc = do
  pure (V3 (2.0 * (i * k - w * j))
          (2.0 * (w * i + j * k))
          (2.0 * (0.5 - i * i - j * j))
        - normalizeV3 acc)

-- buildJacobian :: Quaternion IFloat ->

madgwickTypes :: Module
madgwickTypes = package (named "madgwicktypes") $ do
  defStruct (Proxy :: Proxy "madgwick_imu") 

madgwickMonitor :: Monitor e AttEstimator
madgwickMonitor = do
    madgwickState <- state (named "madgwick_state")
    monitorModuleDef $ do
        incl (madgwickInit madgwickState)
        incl (madgwickUpdate madgwickState)
    
    return AttEstimator
        {   madgwick_init = call_ (madgwickInit madgwickState)
        ,   madgwick_update = call_ (madgwickUpdate madgwickState)
        ,   madgwick_state = madgwickState
        }

sensorFusion
    :: ChanOutput (Struct "imu_sample")
    -> ChanOutput (Stored ITime)
    -> Tower e
        (ChanOutput ('Struct "madgwick_imu"))

sensorFusion samplesOut initOut = do
    towerModule madgwickTypes
    towerDepends madgwickTypes

    (inc, orientationOut) <- channel

    monitor (named "sensor_fusion") $ do

        attitude <- madgwickMonitor

        handler initOut (named "initialize") $ do
            callback $ const $ do
                madgwick_init attitude

        handler samplesOut (named "gotsample") $ do
            o <- emitter inc 1
            callback $ \s -> do
                madgwick_update attitude s

                emit o (constRef (madgwick_state attitude))
    
    pure orientationOut


madgwickTower 
    -- + 6 dof parameters 
    :: ChanOutput (Struct "imu_sample")
    -> Tower e
        (ChanOutput (Struct "orientation_delta"))

madgwickTower samplesOut = do
    towerModule madgwickTypes
    towerDepends madgwickTypes

    (inc, out) <- channel
    
    monitor (named "madgwick") $ do
        -- ref to our madgwick_imu struct

        handler samplesOut (named "handle_sample") $ do
            o <- emitter inc 1
            callback $ \s -> do

                delta <- local $ istruct
                    [   droll .= ival 1.0
                    ,   dpitch .= ival 2.0
                    ,   dyaw .= ival 3.0
                    ]

                emit o (constRef delta)

    pure out

named :: String -> String
named n = n ++ "madgwicktower"