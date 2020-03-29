{-# LANGUAGE PatternSynonyms #-}

module Chemistry.Core where

data Element
  = H -- ^ The /hydrogen/ element.
  | He -- ^ The /helium/ element.
  | Li -- ^ The /lithium/ element.
  | Be -- ^ The /beryllium/ element.
  | B -- ^ The /boron/ element.
  | C -- ^ The /carbon/ element.
  | N -- ^ The /nitrogen/ element.
  | O -- ^ The /oxygen/ element.
  | F -- ^ The /fluorine/ element.
  | Ne -- ^ The /neon/ element.
  | Na -- ^ The /sodium/ element.
  | Mg -- ^ The /magnesium/ element.
  | Al -- ^ The /aluminium/ element.
  | Si -- ^ The /silicon/ element.
  | P -- ^ The /phosphorus/ element.
  | S -- ^ The /sulfur/ element.
  | Cl -- ^ The /chlorine/ element.
  | Ar -- ^ The /argon/ element.
  | K -- ^ The /potassium/ element.
  | Ca -- ^ The /calcium/ element.
  | Sc -- ^ The /scandium/ element.
  | Ti -- ^ The /titanium/ element.
  | V -- ^ The /vanadium/ element.
  | Cr -- ^ The /chromium/ element.
  | Mn -- ^ The /manganese/ element.
  | Fe -- ^ The /iron/ element.
  | Co -- ^ The /cobalt/ element.
  | Ni -- ^ The /nickel/ element.
  | Cu -- ^ The /copper/ element.
  | Zn -- ^ The /zinc/ element.
  | Ga -- ^ The /gallium/ element.
  | Ge -- ^ The /germanium/ element.
  | As -- ^ The /arsenic/ element.
  | Se -- ^ The /selenium/ element.
  | Br -- ^ The /bromine/ element.
  | Kr -- ^ The /krypton/ element.
  | Rb -- ^ The /rubidium/ element.
  | Sr -- ^ The /strontium/ element.
  | Y -- ^ The /yttrium/ element.
  | Zr -- ^ The /zirconium/ element.
  | Nb -- ^ The /niobium/ element.
  | Mo -- ^ The /molybdenum/ element.
  | Tc -- ^ The /technetium/ element.
  | Ru -- ^ The /ruthenium/ element.
  | Rh -- ^ The /rhodium/ element.
  | Pd -- ^ The /palladium/ element.
  | Ag -- ^ The /silver/ element.
  | Cd -- ^ The /cadmium/ element.
  | In -- ^ The /indium/ element.
  | Sn -- ^ The /tin/ element.
  | Sb -- ^ The /antimony/ element.
  | Te -- ^ The /tellurium/ element.
  | I -- ^ The /iodine/ element.
  | Xe -- ^ The /xenon/ element.
  | Cs -- ^ The /caesium/ element.
  | Ba -- ^ The /barium/ element.
  | La -- ^ The /lanthanum/ element.
  | Ce -- ^ The /cerium/ element.
  | Pr -- ^ The /praseodymium/ element.
  | Nd -- ^ The /neodymium/ element.
  | Pm -- ^ The /promethium/ element.
  | Sm -- ^ The /samarium/ element.
  | Eu -- ^ The /europium/ element.
  | Gd -- ^ The /gadolinium/ element.
  | Tb -- ^ The /terbium/ element.
  | Dy -- ^ The /dysprosium/ element.
  | Ho -- ^ The /holmium/ element.
  | Er -- ^ The /erbium/ element.
  | Tm -- ^ The /thulium/ element.
  | Yb -- ^ The /ytterbium/ element.
  | Lu -- ^ The /lutetium/ element.
  | Hf -- ^ The /hafnium/ element.
  | Ta -- ^ The /tantalum/ element.
  | W -- ^ The /tungsten/ element.
  | Re -- ^ The /rhenium/ element.
  | Os -- ^ The /osmium/ element.
  | Ir -- ^ The /iridium/ element.
  | Pt -- ^ The /platinum/ element.
  | Au -- ^ The /gold/ element.
  | Hg -- ^ The /mercury/ element.
  | Tl -- ^ The /thallium/ element.
  | Pb -- ^ The /lead/ element.
  | Bi -- ^ The /bismuth/ element.
  | Po -- ^ The /polonium/ element.
  | At -- ^ The /astatine/ element.
  | Rn -- ^ The /radon/ element.
  | Fr -- ^ The /francium/ element.
  | Ra -- ^ The /radium/ element.
  | Ac -- ^ The /actinium/ element.
  | Th -- ^ The /thorium/ element.
  | Pa -- ^ The /protactinium/ element.
  | U -- ^ The /uranium/ element.
  | Np -- ^ The /neptunium/ element.
  | Pu -- ^ The /plutonium/ element.
  | Am -- ^ The /americium/ element.
  | Cm -- ^ The /curium/ element.
  | Bk -- ^ The /berkelium/ element.
  | Cf -- ^ The /californium/ element.
  | Es -- ^ The /einsteinium/ element.
  | Fm -- ^ The /fermium/ element.
  | Md -- ^ The /mendelevium/ element.
  | No -- ^ The /nobelium/ element.
  | Lr -- ^ The /lawrencium/ element.
  | Rf -- ^ The /rutherfordium/ element.
  | Db -- ^ The /dubnium/ element.
  | Sg -- ^ The /seaborgium/ element.
  | Bh -- ^ The /bohrium/ element.
  | Hs -- ^ The /hassium/ element.
  | Mt -- ^ The /meitnerium/ element.
  | Ds -- ^ The /darmstadtium/ element.
  | Rg -- ^ The /roentgenium/ element.
  | Cn -- ^ The /copernicium/ element.
  | Nh -- ^ The /nihonium/ element.
  | Fl -- ^ The /flerovium/ element.
  | Mc -- ^ The /moscovium/ element.
  | Lv -- ^ The /livermorium/ element.
  | Ts -- ^ The /tennessine/ element.
  | Og -- ^ The /oganesson/ element.
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | A pattern synonym for /unnilunium/, which is the systematic element name of /mendelevium/.
pattern Unu :: Element
pattern Unu = Md

-- | A pattern synonym for /unnilbium/, which is the systematic element name of /nobelium/.
pattern Unb :: Element
pattern Unb = No

-- | A pattern synonym for /unniltrium/, which is the systematic element name of /lawrencium/.
pattern Unt :: Element
pattern Unt = Lr

-- | A pattern synonym for /unnilquadium/, which is the systematic element name of /rutherfordium/.
pattern Unq :: Element
pattern Unq = Rf

-- | A pattern synonym for /unnilpentium/, which is the systematic element name of /dubnium/.
pattern Unp :: Element
pattern Unp = Db

-- | A pattern synonym for /unnilhexium/, which is the systematic element name of /seaborgium/.
pattern Unh :: Element
pattern Unh = Sg

-- | A pattern synonym for /unnilseptium/, which is the systematic element name of /bohrium/.
pattern Uns :: Element
pattern Uns = Bh

-- | A pattern synonym for /unniloctium/, which is the systematic element name of /hassium/.
pattern Uno :: Element
pattern Uno = Hs

-- | A pattern synonym for /unnilennium/, which is the systematic element name of /meitnerium/.
pattern Une :: Element
pattern Une = Mt

-- | A pattern synonym for /ununnilium/, which is the systematic element name of /darmstadtium/.
pattern Uun :: Element
pattern Uun = Ds

-- | A pattern synonym for /unununium/, which is the systematic element name of /roentgenium/.
pattern Uuu :: Element
pattern Uuu = Rg

-- | A pattern synonym for /ununbium/, which is the systematic element name of /copernicium/.
pattern Uub :: Element
pattern Uub = Cn

-- | A pattern synonym for /ununtrium/, which is the systematic element name of /nihonium/.
pattern Uut :: Element
pattern Uut = Nh

-- | A pattern synonym for /ununquadium/, which is the systematic element name of /flerovium/.
pattern Uuq :: Element
pattern Uuq = Fl

-- | A pattern synonym for /ununpentium/, which is the systematic element name of /moscovium/.
pattern Uup :: Element
pattern Uup = Mc

-- | A pattern synonym for /ununhexium/, which is the systematic element name of /livermorium/.
pattern Uuh :: Element
pattern Uuh = Lv

-- | A pattern synonym for /ununseptium/, which is the systematic element name of /tennessine/.
pattern Uus :: Element
pattern Uus = Ts

-- | A pattern synonym for /ununoctium/, which is the systematic element name of /oganesson/.
pattern Uuo :: Element
pattern Uuo = Og

-- | Obtain the atomic number of the given 'Element'.
atomNumber :: Element -> Int
atomNumber = (1+) . fromEnum
