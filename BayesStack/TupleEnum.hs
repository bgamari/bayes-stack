module BayesStack.TupleEnum where

-- This is probably a bad idea
-- Perhaps some TH magic to automatically derive this would make it more tolerable
instance (Enum a, Enum b) => Enum (a,b) where
  fromEnum (a,b) = 2^32 * fromEnum a + fromEnum b
  toEnum n = let (na, nb) = n `quotRem` (2^32)
             in (toEnum na, toEnum nb)

