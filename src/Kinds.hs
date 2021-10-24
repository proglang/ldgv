module Kinds where

data Multiplicity = MMany | MOne
  deriving (Show, Eq, Ord)

data Occurrence = Many | One | Zero
  deriving (Show,Eq)

inject :: Multiplicity -> Occurrence
inject MMany = Many
inject MOne = One

class Demote a where
  demote :: a -> Occurrence

instance Demote Multiplicity where
  demote MMany = Many
  demote MOne  = Zero

instance Demote Occurrence where
  demote Many = Many
  demote One  = Zero
  demote Zero = Zero


data Kind = Kun | Klin | Kunit | Kssn | Kidx
  deriving (Show, Read, Eq)

klub :: Kind -> Kind -> Kind
klub Klin k = Klin
klub k Klin = Klin
klub Kunit Kidx = Kun
klub Kidx Kunit = Kun
klub k Kunit = k
klub Kunit k = k
klub Kun Kun = Kun
klub Kun Kssn = Klin
klub Kssn Kun = Klin
klub Kun Kidx = Kun
klub Kidx Kun = Kun
klub Kssn Kssn = Kssn

-- natural multiplicity
mult :: Kind -> Multiplicity
mult Kun = MMany
mult Klin = MOne
mult Kunit = MMany
mult Kssn = MOne
mult Kidx = MMany

use :: Occurrence -> Maybe Occurrence
use Zero = Nothing
use One = Just Zero
use Many = Just Many

olub :: Occurrence -> Occurrence -> Occurrence
olub One _ = One
olub _ One = One
olub Many Many = Many

kindof :: Multiplicity -> Kind
kindof MOne = Klin
kindof MMany = Kun

kolub :: (Kind, Multiplicity) -> (Kind, Multiplicity) -> (Kind, Multiplicity)
kolub (k1,o1) (k2,o2) = (klub k1 k2, max o1 o2)
