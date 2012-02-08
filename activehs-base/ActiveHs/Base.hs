{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module ActiveHs.Base
    ( toDyn
    , Dynamic
    , WrapData (WrapData)
    , wrapData
    , WrapData2 (WrapData2)
    , Data

    , TestCase (TestCase)

    , QCInt  (QCInt)
    , QCNat  (QCNat)
    , QCBool (QCBool)
    ) where

import Data.Dynamic (Dynamic, toDyn)
import Data.Data (Data, Typeable)
import Test.QuickCheck

-------------------------

data WrapData
    = forall a. Data a 
    => WrapData a
        deriving (Typeable)

data WrapData2
    = forall a. Data a
    => WrapData2 a a
        deriving (Typeable)

wrapData :: Data a => a -> WrapData
wrapData = WrapData

------------------------

data TestCase 
    = forall a prop . (Testable prop, Data a) 
    => TestCase  (((String, a, a) -> Property) -> prop)
        deriving (Typeable)

newtype QCInt = QCInt Int
    deriving (Show, Arbitrary)

newtype QCBool = QCBool Bool
    deriving (Show, Arbitrary)

newtype QCNat = QCNat Int
    deriving (Show)

instance Arbitrary QCNat where
    arbitrary 
        = fmap (\(NonNegative n) -> QCNat n) arbitrary
    shrink (QCNat n) 
        = fmap (\(NonNegative m) -> QCNat m) $ shrink (NonNegative n)

