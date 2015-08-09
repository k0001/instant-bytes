{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck as QC

import qualified Data.Bytes.Serial as Bytes
import qualified Data.Bytes.Put as Bytes
import qualified Data.Bytes.Get as Bytes
import Data.Proxy

import qualified Generics.Instant as GI
import qualified Generics.Instant.TH as GI
import Generics.Instant.Functions.Bytes (GSerial, RepGSerial,
                                         gserializeDefault, gdeserializeDefault)

--------------------------------------------------------------------------------
-- orphans

data Unit_Unit_
instance GI.Constructor Unit_Unit_ where conName _ = "()"
instance GI.Representable () where
  type Rep () = GI.U
  from () = GI.U
  to GI.U = ()

instance Arbitrary (Proxy a) where
  arbitrary = return Proxy
instance Bytes.Serial (Proxy a) where
  serialize _ = Bytes.serialize ()
  deserialize = Bytes.deserialize >>= \() -> return Proxy

--------------------------------------------------------------------------------
-- many constructors, recursive

data ZZ = ZZ1 Int | ZZ2 Char | ZZ3 ZZ deriving (Show, Eq)
GI.deriveAll ''ZZ
instance Bytes.Serial ZZ where
  serialize = gserializeDefault
  deserialize = gdeserializeDefault
instance Arbitrary ZZ where
  arbitrary = QC.oneof [ ZZ1 <$> QC.arbitrary
                       , ZZ2 <$> QC.arbitrary
                       , ZZ3 <$> QC.arbitrary ]

--------------------------------------------------------------------------------
-- GADT

data Foo1 (a :: *) :: * where
  Foo1_1 :: Bool -> Foo1 Bool
  Foo1_2 :: a -> Foo1 a
GI.deriveAll ''Foo1
deriving instance Eq a => Eq (Foo1 a)
deriving instance Show a => Show (Foo1 a)
instance QC.Arbitrary (Foo1 Bool) where
  arbitrary = Foo1_1 <$> QC.arbitrary
instance {-# OVERLAPPABLE #-} (QC.Arbitrary a, GI.Representable a) => QC.Arbitrary (Foo1 a) where
  arbitrary = Foo1_2 <$> QC.arbitrary


data Foo2 (a :: *) (b :: *) :: * where
  Foo2_1 :: a -> Char -> Foo2 a Int
  Foo2_2 :: a -> b -> Foo2 a b
GI.deriveAll ''Foo2
deriving instance (Eq a, Eq b) => Eq (Foo2 a b)
deriving instance (Show a, Show b) => Show (Foo2 a b)
instance (QC.Arbitrary a, GI.Representable a) => QC.Arbitrary (Foo2 a Int) where
  arbitrary = Foo2_1 <$> QC.arbitrary <*> QC.arbitrary
instance {-# OVERLAPPABLE #-} (QC.Arbitrary a, GI.Representable a, QC.Arbitrary b, GI.Representable b) => QC.Arbitrary (Foo2 a b) where
  arbitrary = Foo2_2 <$> QC.arbitrary <*> QC.arbitrary


data Bar1 (a :: Bool) :: * where
  Bar1_1 :: Char -> Bar1 'True
  Bar1_2 :: Int -> Bar1 'False
GI.deriveAll ''Bar1
deriving instance Eq (Bar1 a)
deriving instance Show (Bar1 a)
instance QC.Arbitrary (Bar1 'True) where
  arbitrary = Bar1_1 <$> QC.arbitrary
instance QC.Arbitrary (Bar1 'False) where
  arbitrary = Bar1_2 <$> QC.arbitrary

data Bar2 (a :: k1) (b :: Bool) :: * where
  Bar2_1 :: Int -> Proxy a -> Bar2 a 'True
  Bar2_2 :: String -> Proxy a -> Bar2 a 'False
GI.deriveAll ''Bar2
deriving instance Eq (Bar2 a b)
deriving instance Show (Bar2 a b)
instance (QC.Arbitrary a) => QC.Arbitrary (Bar2 a 'True) where
  arbitrary = Bar2_1 <$> QC.arbitrary <*> QC.arbitrary
instance (QC.Arbitrary a) => QC.Arbitrary (Bar2 a 'False) where
  arbitrary = Bar2_2 <$> QC.arbitrary <*> QC.arbitrary


--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "QuickCheck - prop_IdGSerial"
  [ -- QC.testProperty "()" (prop_IdGSerial :: () -> Bool)
--   , QC.testProperty "Bool" (prop_IdGSerial :: Bool -> Bool)
--   , QC.testProperty "Char" (prop_IdGSerial :: Char -> Bool)
--   , QC.testProperty "Float" (prop_IdGSerial :: Float -> Bool)
--   , QC.testProperty "Int" (prop_IdGSerial :: Int -> Bool)
    QC.testProperty "Maybe ()" (prop_IdGSerial :: Maybe () -> Bool)
  , QC.testProperty "Maybe Bool" (prop_IdGSerial :: Maybe Bool -> Bool)
  , QC.testProperty "Maybe Char" (prop_IdGSerial :: Maybe Char -> Bool)
  , QC.testProperty "Maybe Float" (prop_IdGSerial :: Maybe Float -> Bool)
  , QC.testProperty "Maybe Int" (prop_IdGSerial :: Maybe Int -> Bool)
  , QC.testProperty "Maybe ZZ" (prop_IdGSerial :: Maybe ZZ -> Bool)
  , QC.testProperty "[()]" (prop_IdGSerial :: [()] -> Bool)
  , QC.testProperty "[Bool]" (prop_IdGSerial :: [Bool] -> Bool)
  , QC.testProperty "[Char]" (prop_IdGSerial :: [Char] -> Bool)
  , QC.testProperty "[Float]" (prop_IdGSerial :: [Float] -> Bool)
  , QC.testProperty "[Int]" (prop_IdGSerial :: [Int] -> Bool)
  , QC.testProperty "[ZZ]" (prop_IdGSerial :: [ZZ] -> Bool)
  , QC.testProperty "Foo1 Int" (prop_IdGSerial :: Foo1 Int -> Bool)
  , QC.testProperty "Foo1 Char" (prop_IdGSerial :: Foo1 Char -> Bool)
  , QC.testProperty "Foo2 Float Int" (prop_IdGSerial :: Foo2 Float Int -> Bool)
  , QC.testProperty "Foo2 Bool Char" (prop_IdGSerial :: Foo2 Bool Char -> Bool)
  , QC.testProperty "Bar1 'True" (prop_IdGSerial :: Bar1 'True -> Bool)
  , QC.testProperty "Bar1 'False" (prop_IdGSerial :: Bar1 'False -> Bool)
  , QC.testProperty "Bar2 Int 'True" (prop_IdGSerial :: Bar2 Int 'True -> Bool)
  , QC.testProperty "Bar2 Float 'False" (prop_IdGSerial :: Bar2 Float 'False -> Bool)
  ]

prop_IdGSerial :: (Eq a, Show a, RepGSerial a) => a -> Bool
prop_IdGSerial a =
  a == Bytes.runGetL gdeserializeDefault (Bytes.runPutL (gserializeDefault a))
