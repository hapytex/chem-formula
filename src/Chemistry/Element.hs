{-# LANGUAGE PatternSynonyms #-}

module Chemistry.Element where

import Data.Hashable(Hashable(hashWithSalt))
import Data.Ix(Ix(range, index, inRange, rangeSize))

import Numeric.Units.Dimensional(DMass, Quantity, (*~))
import Numeric.Units.Dimensional.NonSI (dalton)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), arbitraryBoundedEnum)

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
atomNumber :: Element -- ^ The element for which we want to calculate the atomic number.
    -> Int -- ^ The atomic number of the given element.
atomNumber = (1+) . fromEnum

-- | Obtain the symbol of the given 'Element'.
symbol :: Element -- ^ The given element for which we want to obtain the symbol.
    -> String -- ^ The symbol of the given element.
symbol = show

-- | Obtain the name of the given 'Element'.
elementName :: Element -- ^ The element for which we want to obtain the name.
    -> String -- ^ The name of the given element.
elementName H = "hydrogen"
elementName He = "helium"
elementName Li = "lithium"
elementName Be = "beryllium"
elementName B = "boron"
elementName C = "carbon"
elementName N = "nitrogen"
elementName O = "oxygen"
elementName F = "fluorine"
elementName Ne = "neon"
elementName Na = "sodium"
elementName Mg = "magnesium"
elementName Al = "aluminium"
elementName Si = "silicon"
elementName P = "phosphorus"
elementName S = "sulfur"
elementName Cl = "chlorine"
elementName Ar = "argon"
elementName K = "potassium"
elementName Ca = "calcium"
elementName Sc = "scandium"
elementName Ti = "titanium"
elementName V = "vanadium"
elementName Cr = "chromium"
elementName Mn = "manganese"
elementName Fe = "iron"
elementName Co = "cobalt"
elementName Ni = "nickel"
elementName Cu = "copper"
elementName Zn = "zinc"
elementName Ga = "gallium"
elementName Ge = "germanium"
elementName As = "arsenic"
elementName Se = "selenium"
elementName Br = "bromine"
elementName Kr = "krypton"
elementName Rb = "rubidium"
elementName Sr = "strontium"
elementName Y = "yttrium"
elementName Zr = "zirconium"
elementName Nb = "niobium"
elementName Mo = "molybdenum"
elementName Tc = "technetium"
elementName Ru = "ruthenium"
elementName Rh = "rhodium"
elementName Pd = "palladium"
elementName Ag = "silver"
elementName Cd = "cadmium"
elementName In = "indium"
elementName Sn = "tin"
elementName Sb = "antimony"
elementName Te = "tellurium"
elementName I = "iodine"
elementName Xe = "xenon"
elementName Cs = "caesium"
elementName Ba = "barium"
elementName La = "lanthanum"
elementName Ce = "cerium"
elementName Pr = "praseodymium"
elementName Nd = "neodymium"
elementName Pm = "promethium"
elementName Sm = "samarium"
elementName Eu = "europium"
elementName Gd = "gadolinium"
elementName Tb = "terbium"
elementName Dy = "dysprosium"
elementName Ho = "holmium"
elementName Er = "erbium"
elementName Tm = "thulium"
elementName Yb = "ytterbium"
elementName Lu = "lutetium"
elementName Hf = "hafnium"
elementName Ta = "tantalum"
elementName W = "tungsten"
elementName Re = "rhenium"
elementName Os = "osmium"
elementName Ir = "iridium"
elementName Pt = "platinum"
elementName Au = "gold"
elementName Hg = "mercury"
elementName Tl = "thallium"
elementName Pb = "lead"
elementName Bi = "bismuth"
elementName Po = "polonium"
elementName At = "astatine"
elementName Rn = "radon"
elementName Fr = "francium"
elementName Ra = "radium"
elementName Ac = "actinium"
elementName Th = "thorium"
elementName Pa = "protactinium"
elementName U = "uranium"
elementName Np = "neptunium"
elementName Pu = "plutonium"
elementName Am = "americium"
elementName Cm = "curium"
elementName Bk = "berkelium"
elementName Cf = "californium"
elementName Es = "einsteinium"
elementName Fm = "fermium"
elementName Md = "mendelevium"
elementName No = "nobelium"
elementName Lr = "lawrencium"
elementName Rf = "rutherfordium"
elementName Db = "dubnium"
elementName Sg = "seaborgium"
elementName Bh = "bohrium"
elementName Hs = "hassium"
elementName Mt = "meitnerium"
elementName Ds = "darmstadtium"
elementName Rg = "roentgenium"
elementName Cn = "copernicium"
elementName Nh = "nihonium"
elementName Fl = "flerovium"
elementName Mc = "moscovium"
elementName Lv = "livermorium"
elementName Ts = "tennessine"
elementName Og = "oganesson"

-- | Obtain the atomic weight of the given 'Element' given this is specified.
atomicWeight :: Floating a => Element -- ^ The element for which we want to obtain the atomic weight.
    -> Maybe (Quantity DMass a)  -- ^ In case the atomic weight is known, the atomic weight wrapped in a 'Just' data constructor.
atomicWeight H = Just (1.00797 *~ dalton)
atomicWeight He = Just (4.00260 *~ dalton)
atomicWeight Li = Just (6.941 *~ dalton)
atomicWeight Be = Just (9.01218 *~ dalton)
atomicWeight B = Just (10.81 *~ dalton)
atomicWeight C = Just (12.011 *~ dalton)
atomicWeight N = Just (14.0067 *~ dalton)
atomicWeight O = Just (15.9994 *~ dalton)
atomicWeight F = Just (18.998403 *~ dalton)
atomicWeight Ne = Just (20.179 *~ dalton)
atomicWeight Na = Just (22.98977 *~ dalton)
atomicWeight Mg = Just (24.305 *~ dalton)
atomicWeight Al = Just (26.98154 *~ dalton)
atomicWeight Si = Just (28.0855 *~ dalton)
atomicWeight P = Just (30.97376 *~ dalton)
atomicWeight S = Just (32.06 *~ dalton)
atomicWeight Cl = Just (35.453 *~ dalton)
atomicWeight K = Just (39.0983 *~ dalton)
atomicWeight Ar = Just (39.948 *~ dalton)
atomicWeight Ca = Just (40.08 *~ dalton)
atomicWeight Sc = Just (44.9559 *~ dalton)
atomicWeight Ti = Just (47.90 *~ dalton)
atomicWeight V = Just (50.9415 *~ dalton)
atomicWeight Cr = Just (51.996 *~ dalton)
atomicWeight Mn = Just (54.9380 *~ dalton)
atomicWeight Fe = Just (55.847 *~ dalton)
atomicWeight Ni = Just (58.70 *~ dalton)
atomicWeight Co = Just (58.9332 *~ dalton)
atomicWeight Cu = Just (63.546 *~ dalton)
atomicWeight Zn = Just (65.38 *~ dalton)
atomicWeight Ga = Just (69.72 *~ dalton)
atomicWeight Ge = Just (72.59 *~ dalton)
atomicWeight As = Just (74.9216 *~ dalton)
atomicWeight Se = Just (78.96 *~ dalton)
atomicWeight Br = Just (79.904 *~ dalton)
atomicWeight Kr = Just (83.80 *~ dalton)
atomicWeight Rb = Just (85.4678 *~ dalton)
atomicWeight Sr = Just (87.62 *~ dalton)
atomicWeight Y = Just (88.9059 *~ dalton)
atomicWeight Zr = Just (91.22 *~ dalton)
atomicWeight Nb = Just (92.9064 *~ dalton)
atomicWeight Mo = Just (95.94 *~ dalton)
atomicWeight Ru = Just (101.07 *~ dalton)
atomicWeight Rh = Just (102.9055 *~ dalton)
atomicWeight Pd = Just (106.4 *~ dalton)
atomicWeight Ag = Just (107.868 *~ dalton)
atomicWeight Cd = Just (112.41 *~ dalton)
atomicWeight In = Just (114.82 *~ dalton)
atomicWeight Sn = Just (118.69 *~ dalton)
atomicWeight Sb = Just (121.75 *~ dalton)
atomicWeight I = Just (126.9045 *~ dalton)
atomicWeight Te = Just (127.60 *~ dalton)
atomicWeight Xe = Just (131.30 *~ dalton)
atomicWeight Cs = Just (132.9054 *~ dalton)
atomicWeight Ba = Just (137.33 *~ dalton)
atomicWeight La = Just (138.9055 *~ dalton)
atomicWeight Ce = Just (140.12 *~ dalton)
atomicWeight Pr = Just (140.9077 *~ dalton)
atomicWeight Nd = Just (144.24 *~ dalton)
atomicWeight Sm = Just (150.4 *~ dalton)
atomicWeight Eu = Just (151.96 *~ dalton)
atomicWeight Gd = Just (157.25 *~ dalton)
atomicWeight Tb = Just (158.9254 *~ dalton)
atomicWeight Dy = Just (162.50 *~ dalton)
atomicWeight Ho = Just (164.9304 *~ dalton)
atomicWeight Er = Just (167.26 *~ dalton)
atomicWeight Tm = Just (168.9342 *~ dalton)
atomicWeight Yb = Just (173.04 *~ dalton)
atomicWeight Lu = Just (174.967 *~ dalton)
atomicWeight Hf = Just (178.49 *~ dalton)
atomicWeight Ta = Just (180.9479 *~ dalton)
atomicWeight W = Just (183.85 *~ dalton)
atomicWeight Re = Just (186.207 *~ dalton)
atomicWeight Os = Just (190.2 *~ dalton)
atomicWeight Ir = Just (192.22 *~ dalton)
atomicWeight Pt = Just (195.09 *~ dalton)
atomicWeight Au = Just (196.9665 *~ dalton)
atomicWeight Hg = Just (200.59 *~ dalton)
atomicWeight Tl = Just (204.37 *~ dalton)
atomicWeight Pb = Just (207.2 *~ dalton)
atomicWeight Bi = Just (208.9804 *~ dalton)
atomicWeight Ra = Just (226.0254 *~ dalton)
atomicWeight Ac = Just (227.0278 *~ dalton)
atomicWeight Pa = Just (231.0359 *~ dalton)
atomicWeight Th = Just (232.0381 *~ dalton)
atomicWeight Np = Just (237.0482 *~ dalton)
atomicWeight U = Just (238.029 *~ dalton)
atomicWeight _ = Nothing

instance Hashable Element where
    hashWithSalt = (. fromEnum) . hashWithSalt

instance Ix Element where
    range (ea, eb) = [ea .. eb]
    index t@(ea, eb) e | inRange t e = fromEnum e - fromEnum eb
                       | otherwise = error "Out of bounds."
    inRange (ea, eb) e = ea <= e && e <= eb
    rangeSize (ea, eb) = fromEnum eb - fromEnum ea + 1

instance Arbitrary Element where
    arbitrary = arbitraryBoundedEnum
