module Chemistry.Orbit where

import Data.List.NonEmpty(NonEmpty)

data Orbit = S | P | D | F | G deriving (Enum, Eq, Ord, Read, Show)
newtype OrbitalConfig = OrbitalConfig (NonEmpty (NonEmpty Int)) deriving (Eq, Ord, Read, Show)

totalElectrons :: OrbitalConfig -> Int
totalElectrons (OrbitalConfig vs) = sum (fmap sum vs)

toArrowText :: Int -> Int -> String
toArrowText n k = concat (replicate f "\8645\8414" ++ [ "\8593\8414" | h == 1 ] ++ replicate ((maxElectronsForOrbit' n `div` 2) - f - h) " \8414")
  where ~(f, h) = k `divMod` 2

maxElectronsForOrbit' :: Int -> Int
maxElectronsForOrbit' = (2+) . (4*)

maxElectronsForOrbit :: Orbit -> Int
maxElectronsForOrbit = maxElectronsForOrbit' . fromEnum

electronsToOrbitals :: Int -> [(Int, Orbit, Int)]
electronsToOrbitals = fill 0 0
  where fill _ _ 0 = []
        fill m 0 k = (m+1, S, d) : fill (m - (m-1) `div` 2) ((m+1) `div` 2) (k-d)
          where d = min 2 k
        fill m n k = (m+1, toEnum n, d) : fill (m+1) (n-1) (k-d)
          where d = min (maxElectronsForOrbit' n) k

-- electronsPerShell :: Element -> [Int]
-- electronsPerShell = map sum . molecularOrbital

