{-# LANGUAGE PatternSynonyms #-}

module Chemistry.Core where

data Atom
  = H
  | He
  | Li
  | Be
  | B
  | C
  | N
  | O
  | F
  | Ne
  | Na
  | Mg
  | Al
  | Si
  | P
  | S
  | Cl
  | Ar
  | K
  | Ca
  | Sc
  | Ti
  | V
  | Cr
  | Mn
  | Fe
  | Co
  | Ni
  | Cu
  | Zn
  | Ga
  | Ge
  | As
  | Se
  | Br
  | Kr
  | Rb
  | Sr
  | Y
  | Zr
  | Nb
  | Mo
  | Tc
  | Ru
  | Rh
  | Pd
  | Ag
  | Cd
  | In
  | Sn
  | Sb
  | Te
  | I
  | Xe
  | Cs
  | Ba
  | La
  | Ce
  | Pr
  | Nd
  | Pm
  | Sm
  | Eu
  | Gd
  | Tb
  | Dy
  | Ho
  | Er
  | Tm
  | Yb
  | Lu
  | Hf
  | Ta
  | W
  | Re
  | Os
  | Ir
  | Pt
  | Au
  | Hg
  | Tl
  | Pb
  | Bi
  | Po
  | At
  | Rn
  | Fr
  | Ra
  | Ac
  | Th
  | Pa
  | U
  | Np
  | Pu
  | Am
  | Cm
  | Bk
  | Cf
  | Es
  | Fm
  | Md
  | No
  | Lr
  | Rf
  | Db
  | Sg
  | Bh
  | Hs
  | Mt
  | Ds
  | Rg
  | Cn
  | Nh
  | Fl
  | Mc
  | Lv
  | Ts
  | Og
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

pattern Unu :: Atom
pattern Unu = Md

pattern Unb :: Atom
pattern Unb = No

pattern Unt :: Atom
pattern Unt = Lr

pattern Unq :: Atom
pattern Unq = Rf

pattern Unp :: Atom
pattern Unp = Db

pattern Unh :: Atom
pattern Unh = Sg

pattern Uns :: Atom
pattern Uns = Bh

pattern Uno :: Atom
pattern Uno = Hs

pattern Une :: Atom
pattern Une = Mt

pattern Uun :: Atom
pattern Uun = Ds

pattern Uuu :: Atom
pattern Uuu = Rg

pattern Uub :: Atom
pattern Uub = Cn

pattern Uut :: Atom
pattern Uut = Nh

pattern Uuq :: Atom
pattern Uuq = Fl

pattern Uup :: Atom
pattern Uup = Mc

pattern Uuh :: Atom
pattern Uuh = Lv

pattern Uus :: Atom
pattern Uus = Ts

pattern Uuo :: Atom
pattern Uuo = Og
