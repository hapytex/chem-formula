{-# LANGUAGE OverloadedStrings #-}

module Chemistry.Orbit where

import Control.Monad.Zip(mzipWith)
import Data.List(sort)
import Data.List.NonEmpty(NonEmpty((:|)))
import Data.Char.Small(asSup)
import Data.Foldable(toList)
import qualified Data.Text as T
import Data.Text(Text, pack, toLower)

data Orbit = S | P | D | F | G deriving (Enum, Eq, Ord, Read, Show)
data OrbitElement = OrbitElement Int Orbit Int deriving (Eq, Ord, Read, Show)
newtype OrbitalConfig = OrbitalConfig (NonEmpty (NonEmpty Int)) deriving (Eq, Ord, Read, Show)

orbitElementToText :: OrbitElement -> Text
orbitElementToText (OrbitElement i o n) = pack (show i) <> toLower (pack (show o)) <> asSup n

orbitConfigs :: OrbitalConfig -> NonEmpty (NonEmpty OrbitElement)
orbitConfigs (OrbitalConfig es) = mzipWith ((`mzipWith` (S :| [P ..])) . OrbitElement) (1 :| [2..]) es

orbitElement :: Foldable f => f OrbitElement -> OrbitalConfig
orbitElement _ = OrbitalConfig ((0 :| []) :| [])

orbitalConfigToText :: OrbitalConfig -> Text
orbitalConfigToText = T.unwords . toList . fmap (T.unwords . toList . fmap orbitElementToText) . orbitConfigs

totalElectrons :: OrbitalConfig -> Int
totalElectrons (OrbitalConfig vs) = sum (fmap sum vs)

toArrowText :: OrbitElement -> String
toArrowText (OrbitElement m n k) = "" -- concat (replicate f "\8645\8414" ++ [ "\8593\8414" | h == 1 ] ++ replicate ((maxElectronsForOrbit' n `div` 2) - f - h) " \8414")
  -- where ~(f, h) = k `divMod` 2

_orbitPlaces :: Int -> Int
_orbitPlaces = (1+) . (2*)

maxElectronsForOrbit' :: Int -> Int
maxElectronsForOrbit' = (2*) . _orbitPlaces

maxElectronsForOrbit :: Orbit -> Int
maxElectronsForOrbit = maxElectronsForOrbit' . fromEnum

electronsToOrbitals' :: Int -> [OrbitElement]
electronsToOrbitals' = fill 1 0
  where fill _ _ 0 = []
        fill m 0 k = OrbitElement m S d : fill (m - dm + 1) dm (k-d)
          where d = min 2 k
                dm = m `div` 2
        fill m n k = OrbitElement m (toEnum n) d : fill (m+1) (n-1) (k-d)
          where d = min (maxElectronsForOrbit' n) k

electronsToOrbitals :: Int -> [OrbitElement]
electronsToOrbitals = sort . electronsToOrbitals'
