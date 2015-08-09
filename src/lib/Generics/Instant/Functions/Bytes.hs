{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Generics.Instant.Functions.Bytes
  ( -- $defaults
    gserializeDefault
  , gdeserializeDefault
  , RepGSerial
    -- * Internals
  , GSerial(..)
    -- ** Even more internal
  , GSumSerial
  , GSumSize
  ) where

import qualified Data.Bytes.Serial as Bytes
import qualified Data.Bytes.Put as Bytes
import qualified Data.Bytes.Get as Bytes
import Data.Bits
import Data.Word
import Generics.Instant
import Prelude

--------------------------------------------------------------------------------
-- $defaults
--
-- You can use 'gserializeDefault' and 'gdeserializeDefault' as your generic
-- 'Bytes.serialize' and 'Bytes.deserialize' implementations for any 'Representable'
-- type as follows:
--
-- @
-- instance 'Bytes.Serial' MyType where
--    serialize = 'gserializeDefault'
--    deserialize = 'gdeserializeDefault'
-- @

gserializeDefault :: (Representable a, GSerial (Rep a), Bytes.MonadPut m) => a -> m ()
gserializeDefault = \a -> gserialize (from a)
{-# INLINABLE gserializeDefault #-}

gdeserializeDefault :: (Representable a, GSerial (Rep a), Bytes.MonadGet m) => m a
gdeserializeDefault = fmap to gdeserialize
{-# INLINABLE gdeserializeDefault #-}

--------------------------------------------------------------------------------

-- | @'RepGSerial'@ is simply a synonym for
-- @('Representable' a, 'GSerial' ('Rep' a))@ with the convenient
-- kind @(* -> 'GHC.Exts.Constraint')@
class (Representable a, GSerial (Rep a)) => RepGSerial a
instance (Representable a, GSerial (Rep a)) => RepGSerial a

--------------------------------------------------------------------------------

class GSerial a where
  gserialize :: Bytes.MonadPut m => a -> m ()
  gdeserialize :: Bytes.MonadGet m => m a

instance GSerial Z where
  gserialize _ = fail "Generics.Instant.Functions.Serial.GSerial Z gserialize - impossible"
  gdeserialize = fail "Generics.Instant.Functions.Serial.GSerial Z gdeserialize - impossible"

instance GSerial U where
  gserialize U = Bytes.serialize ()
  {-# INLINABLE gserialize #-}
  gdeserialize = Bytes.deserialize >>= \() -> return U
  {-# INLINABLE gdeserialize #-}

instance GSerial a => GSerial (CEq c p p a) where
  gserialize (C a) = gserialize a
  {-# INLINABLE gserialize #-}
  gdeserialize = gdeserialize >>= \a -> return (C a)
  {-# INLINABLE gdeserialize #-}

instance {-# OVERLAPPABLE #-} GSerial a => GSerial (CEq c p q a) where
  gserialize (C a) = gserialize a
  {-# INLINABLE gserialize #-}
  gdeserialize = fail "Generics.Instant.Functions.Serial.GSerial (CEq c p q a) gdeserialize - impossible"

instance Bytes.Serial a => GSerial (Var a) where
  gserialize (Var a) = Bytes.serialize a
  {-# INLINABLE gserialize #-}
  gdeserialize = Bytes.deserialize >>= \a -> return (Var a)
  {-# INLINABLE gdeserialize #-}

instance Bytes.Serial a => GSerial (Rec a) where
  gserialize (Rec a) = Bytes.serialize a
  {-# INLINABLE gserialize #-}
  gdeserialize = Bytes.deserialize >>= \a -> return (Rec a)
  {-# INLINABLE gdeserialize #-}

instance (GSerial a, GSerial b) => GSerial (a :*: b) where
  gserialize (a :*: b) = gserialize a >> gserialize b
  {-# INLINABLE gserialize #-}
  gdeserialize = gdeserialize >>= \a ->
                  gdeserialize >>= \b ->
                  return (a :*: b)
  {-# INLINABLE gdeserialize #-}

---

-- Borrowed from the "binary" package, which borrowed this from "cereal".

-- The following GSerial instance for sums has support for serializing
-- types with up to 2^64-1 constructors. It will use the minimal
-- number of bytes needed to encode the constructor. For example when
-- a type has 2^8 constructors or less it will use a single byte to
-- encode the constructor. If it has 2^16 constructors or less it will
-- use two bytes, and so on till 2^64-1.

instance
  ( GSumSerial a, GSumSerial b, GSerial a, GSerial b, GSumSize a, GSumSize b
  ) => GSerial (a :+: b)
  where
    {-# INLINABLE gserialize #-}
    gserialize x
      | predSize <= fromIntegral (maxBound :: Word8)
          = putSum (0 :: Word8) (fromIntegral size) x
      | predSize <= fromIntegral (maxBound :: Word16)
          = putSum (0 :: Word16) (fromIntegral size) x
      | predSize <= fromIntegral (maxBound :: Word32)
          = putSum (0 :: Word32) (fromIntegral size) x
      | predSize <= fromIntegral (maxBound :: Word64)
          = putSum (0 :: Word64) (fromIntegral size) x
      | otherwise = sizeError "encode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
        predSize = size - 1

    {-# INLINABLE gdeserialize #-}
    gdeserialize
      | predSize <= fromIntegral (maxBound :: Word8)
          = Bytes.deserialize >>= \(c :: Word8) ->
            checkGetSum (fromIntegral size) c
      | predSize <= fromIntegral (maxBound :: Word16)
          = Bytes.deserialize >>= \(c :: Word16) ->
            checkGetSum (fromIntegral size) c
      | predSize <= fromIntegral (maxBound :: Word32)
          = Bytes.deserialize >>= \(c :: Word32) ->
            checkGetSum (fromIntegral size) c
      | predSize <= fromIntegral (maxBound :: Word64)
          = Bytes.deserialize >>= \(c :: Word64) ->
            checkGetSum (fromIntegral size) c
      | otherwise = sizeError "decode" size
      where
        size = unTagged (sumSize :: Tagged (a :+: b) Word64)
        predSize = size - 1

sizeError :: Show size => String -> size -> error
sizeError s size =
  error $ "Generics.Instant.Functions.Serial.sizeError: Can't " ++ s ++ " a type with " ++
          show size ++ " constructors"

------------------------------------------------------------------------

checkGetSum
  :: (Ord w, Num w, Bits w, GSumSerial a, Bytes.MonadGet m) => w -> w -> m a
checkGetSum size code
  | code < size = getSum code size
  | otherwise = fail "Generics.Instant.Functions.Serial.checkGetSum: Unknown encoding for constructor"
{-# INLINABLE checkGetSum #-}

class GSumSerial a where
  putSum :: (Num w, Bits w, Bytes.Serial w, Bytes.MonadPut m) => w -> w -> a -> m ()
  getSum :: (Ord w, Num w, Bits w, Bytes.MonadGet m) => w -> w -> m a

instance (GSumSerial a, GSumSerial b, GSerial a, GSerial b) => GSumSerial (a :+: b) where
  {-# INLINABLE putSum #-}
  putSum !code !size x =
    let sizeL = size `shiftR` 1
        sizeR = size - sizeL
    in case x of
         L l -> putSum code           sizeL l
         R r -> putSum (code + sizeL) sizeR r
  {-# INLINABLE getSum #-}
  getSum !code !size
    | code < sizeL = L <$> getSum code           sizeL
    | otherwise    = R <$> getSum (code - sizeL) sizeR
    where
      sizeL = size `shiftR` 1
      sizeR = size - sizeL

instance GSerial a => GSumSerial (CEq c p p a) where
  putSum !code _ ca = Bytes.serialize code >> gserialize ca
  {-# INLINABLE putSum #-}
  getSum _ _ = gdeserialize
  {-# INLINABLE getSum #-}

instance {-# OVERLAPPABLE #-} GSerial a => GSumSerial (CEq c p q a) where
  putSum !code _ ca = Bytes.serialize code >> gserialize ca
  {-# INLINABLE putSum #-}
  getSum _ _ = fail "Generics.Instant.Functions.Serial.GSumSerial (CEq c p q a) getSum - impossible"

------------------------------------------------------------------------

class GSumSize a where
  sumSize :: Tagged a Word64

newtype Tagged s b = Tagged {unTagged :: b}

instance (GSumSize a, GSumSize b) => GSumSize (a :+: b) where
  {-# INLINABLE sumSize #-}
  sumSize = Tagged (unTagged (sumSize :: Tagged a Word64) +
                    unTagged (sumSize :: Tagged b Word64))

instance GSumSize (CEq c p q a) where
  {-# INLINABLE sumSize #-}
  sumSize = Tagged 1
