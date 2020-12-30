{-# LANGUAGE DeriveTraversable, FlexibleInstances, OverloadedStrings #-}

module Chemistry.Isotope where

import Chemistry.Core(FormulaElement(toFormulaPrec), HillCompare(hillCompare), Weight(weight), showParen')
import Chemistry.Element(Element(..))

import Data.Char.Small(asSup)
import Data.Function(on)
import Data.Text(Text, cons)

import Numeric.Units.Dimensional(DMass, Quantity, (*~))
import Numeric.Units.Dimensional.NonSI (dalton)

import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary), Arbitrary1(liftArbitrary), arbitrary1)

data Isotope a
  = Isotope { isoElement :: a,  massNumber :: Int }
  deriving (Eq, Foldable, Functor, Ord, Read, Show, Traversable)

instance HillCompare a => HillCompare (Isotope a) where
    hillCompare = hillCompare `on` isoElement

_charge :: Int -> Text
_charge 0 = ""
_charge (-1) = "\x207a"
_charge n | n < 0 = asSup n
          | otherwise = cons '\x207a' (asSup n)

isotopeWeight :: Floating a => Element -> Int -> Maybe (Quantity DMass a)
isotopeWeight H 1 = Just (1.007 *~ dalton)
isotopeWeight H 2 = Just (2.014 *~ dalton)
isotopeWeight H 3 = Just (3.016 *~ dalton)
isotopeWeight He 3 = Just (3.016 *~ dalton)
isotopeWeight He 4 = Just (4.002 *~ dalton)
isotopeWeight Li 6 = Just (6.015 *~ dalton)
isotopeWeight Li 7 = Just (7.016 *~ dalton)
isotopeWeight Be 9 = Just (9.012 *~ dalton)
isotopeWeight B 10 = Just (10.012 *~ dalton)
isotopeWeight B 11 = Just (11.009 *~ dalton)
isotopeWeight C 12 = Just (12.000000000 *~ dalton)
isotopeWeight C 13 = Just (13.003 *~ dalton)
isotopeWeight C 14 = Just (14.003 *~ dalton)
isotopeWeight N 14 = Just (14.003 *~ dalton)
isotopeWeight N 15 = Just (15.000 *~ dalton)
isotopeWeight O 16 = Just (15.994 *~ dalton)
isotopeWeight O 17 = Just (16.999 *~ dalton)
isotopeWeight O 18 = Just (17.999 *~ dalton)
isotopeWeight F 19 = Just (18.998 *~ dalton)
isotopeWeight Ne 20 = Just (19.992 *~ dalton)
isotopeWeight Ne 21 = Just (20.993 *~ dalton)
isotopeWeight Ne 22 = Just (21.991 *~ dalton)
isotopeWeight Na 23 = Just (22.989 *~ dalton)
isotopeWeight Mg 24 = Just (23.985 *~ dalton)
isotopeWeight Mg 25 = Just (24.985 *~ dalton)
isotopeWeight Mg 26 = Just (25.982 *~ dalton)
isotopeWeight Al 27 = Just (26.981 *~ dalton)
isotopeWeight Si 28 = Just (27.976 *~ dalton)
isotopeWeight Si 29 = Just (28.976 *~ dalton)
isotopeWeight Si 30 = Just (29.973 *~ dalton)
isotopeWeight P 31 = Just (30.973 *~ dalton)
isotopeWeight S 32 = Just (31.972 *~ dalton)
isotopeWeight S 33 = Just (32.971 *~ dalton)
isotopeWeight S 34 = Just (33.967 *~ dalton)
isotopeWeight S 36 = Just (35.967 *~ dalton)
isotopeWeight Cl 35 = Just (34.968 *~ dalton)
isotopeWeight Cl 37 = Just (36.965 *~ dalton)
isotopeWeight Ar 36 = Just (35.967 *~ dalton)
isotopeWeight Ar 38 = Just (37.962 *~ dalton)
isotopeWeight Ar 40 = Just (39.962 *~ dalton)
isotopeWeight K 39 = Just (38.963 *~ dalton)
isotopeWeight K 40 = Just (39.963 *~ dalton)
isotopeWeight K 41 = Just (40.961 *~ dalton)
isotopeWeight Ca 40 = Just (39.962 *~ dalton)
isotopeWeight Ca 42 = Just (41.958 *~ dalton)
isotopeWeight Ca 43 = Just (42.958 *~ dalton)
isotopeWeight Ca 44 = Just (43.955 *~ dalton)
isotopeWeight Ca 46 = Just (45.953 *~ dalton)
isotopeWeight Ca 48 = Just (47.952 *~ dalton)
isotopeWeight Sc 45 = Just (44.955 *~ dalton)
isotopeWeight Ti 46 = Just (45.952 *~ dalton)
isotopeWeight Ti 47 = Just (46.951 *~ dalton)
isotopeWeight Ti 48 = Just (47.947 *~ dalton)
isotopeWeight Ti 49 = Just (48.947 *~ dalton)
isotopeWeight Ti 50 = Just (49.944 *~ dalton)
isotopeWeight V 50 = Just (49.947 *~ dalton)
isotopeWeight V 51 = Just (50.943 *~ dalton)
isotopeWeight Cr 50 = Just (49.946 *~ dalton)
isotopeWeight Cr 52 = Just (51.940 *~ dalton)
isotopeWeight Cr 53 = Just (52.940 *~ dalton)
isotopeWeight Cr 54 = Just (53.938 *~ dalton)
isotopeWeight Mn 55 = Just (54.938 *~ dalton)
isotopeWeight Fe 54 = Just (53.939 *~ dalton)
isotopeWeight Fe 56 = Just (55.934 *~ dalton)
isotopeWeight Fe 57 = Just (56.935 *~ dalton)
isotopeWeight Fe 58 = Just (57.933 *~ dalton)
isotopeWeight Co 59 = Just (58.933 *~ dalton)
isotopeWeight Ni 58 = Just (57.935 *~ dalton)
isotopeWeight Ni 60 = Just (59.930 *~ dalton)
isotopeWeight Ni 61 = Just (60.931 *~ dalton)
isotopeWeight Ni 62 = Just (61.928 *~ dalton)
isotopeWeight Ni 64 = Just (63.927 *~ dalton)
isotopeWeight Cu 63 = Just (62.929 *~ dalton)
isotopeWeight Cu 65 = Just (64.927 *~ dalton)
isotopeWeight Zn 64 = Just (63.929 *~ dalton)
isotopeWeight Zn 66 = Just (65.926 *~ dalton)
isotopeWeight Zn 67 = Just (66.927 *~ dalton)
isotopeWeight Zn 68 = Just (67.924 *~ dalton)
isotopeWeight Zn 70 = Just (69.925 *~ dalton)
isotopeWeight Ga 69 = Just (68.925 *~ dalton)
isotopeWeight Ga 71 = Just (70.924 *~ dalton)
isotopeWeight Ge 70 = Just (69.924 *~ dalton)
isotopeWeight Ge 72 = Just (71.922 *~ dalton)
isotopeWeight Ge 73 = Just (72.923 *~ dalton)
isotopeWeight Ge 74 = Just (73.921 *~ dalton)
isotopeWeight As 76 = Just (75.921 *~ dalton)
isotopeWeight As 75 = Just (74.921 *~ dalton)
isotopeWeight Se 74 = Just (73.922 *~ dalton)
isotopeWeight Se 76 = Just (75.919 *~ dalton)
isotopeWeight Se 77 = Just (76.919 *~ dalton)
isotopeWeight Se 78 = Just (77.917 *~ dalton)
isotopeWeight Se 80 = Just (79.916 *~ dalton)
isotopeWeight Se 82 = Just (81.916 *~ dalton)
isotopeWeight Br 79 = Just (78.918 *~ dalton)
isotopeWeight Br 81 = Just (80.916 *~ dalton)
isotopeWeight Kr 78 = Just (77.920 *~ dalton)
isotopeWeight Kr 80 = Just (79.916 *~ dalton)
isotopeWeight Kr 82 = Just (81.913 *~ dalton)
isotopeWeight Kr 83 = Just (82.914 *~ dalton)
isotopeWeight Kr 84 = Just (83.911 *~ dalton)
isotopeWeight Kr 86 = Just (85.910 *~ dalton)
isotopeWeight Rb 85 = Just (84.911 *~ dalton)
isotopeWeight Rb 87 = Just (86.909 *~ dalton)
isotopeWeight Sr 84 = Just (83.913 *~ dalton)
isotopeWeight Sr 86 = Just (85.909 *~ dalton)
isotopeWeight Sr 87 = Just (86.908 *~ dalton)
isotopeWeight Sr 88 = Just (87.905 *~ dalton)
isotopeWeight Y 89 = Just (88.905 *~ dalton)
isotopeWeight Zr 90 = Just (89.904 *~ dalton)
isotopeWeight Zr 91 = Just (90.905 *~ dalton)
isotopeWeight Zr 92 = Just (91.905 *~ dalton)
isotopeWeight Zr 94 = Just (93.906 *~ dalton)
isotopeWeight Zr 96 = Just (95.908 *~ dalton)
isotopeWeight Nb 93 = Just (92.906 *~ dalton)
isotopeWeight Mo 92 = Just (91.906 *~ dalton)
isotopeWeight Mo 94 = Just (93.905 *~ dalton)
isotopeWeight Mo 95 = Just (94.905 *~ dalton)
isotopeWeight Mo 96 = Just (95.904 *~ dalton)
isotopeWeight Mo 97 = Just (96.906 *~ dalton)
isotopeWeight Mo 98 = Just (97.905 *~ dalton)
isotopeWeight Mo 100 = Just (99.907 *~ dalton)
isotopeWeight Tc 97 = Just (96.906 *~ dalton)
isotopeWeight Tc 98 = Just (97.907 *~ dalton)
isotopeWeight Tc 99 = Just (98.906 *~ dalton)
isotopeWeight Ru 96 = Just (95.907 *~ dalton)
isotopeWeight Ru 98 = Just (97.905 *~ dalton)
isotopeWeight Ru 99 = Just (98.905 *~ dalton)
isotopeWeight Ru 100 = Just (99.904 *~ dalton)
isotopeWeight Ru 101 = Just (100.905 *~ dalton)
isotopeWeight Ru 102 = Just (101.904 *~ dalton)
isotopeWeight Ru 104 = Just (103.905 *~ dalton)
isotopeWeight Rh 103 = Just (102.905 *~ dalton)
isotopeWeight Pd 102 = Just (101.905 *~ dalton)
isotopeWeight Pd 104 = Just (103.904 *~ dalton)
isotopeWeight Pd 105 = Just (104.905 *~ dalton)
isotopeWeight Pd 106 = Just (105.903 *~ dalton)
isotopeWeight Pd 108 = Just (107.903 *~ dalton)
isotopeWeight Pd 110 = Just (109.905 *~ dalton)
isotopeWeight Ag 107 = Just (106.905 *~ dalton)
isotopeWeight Ag 109 = Just (108.904 *~ dalton)
isotopeWeight Cd 106 = Just (105.906 *~ dalton)
isotopeWeight Cd 108 = Just (107.904 *~ dalton)
isotopeWeight Cd 110 = Just (109.903 *~ dalton)
isotopeWeight Cd 111 = Just (110.904 *~ dalton)
isotopeWeight Cd 112 = Just (111.902 *~ dalton)
isotopeWeight Cd 113 = Just (112.904 *~ dalton)
isotopeWeight Cd 114 = Just (113.903 *~ dalton)
isotopeWeight Cd 116 = Just (115.904 *~ dalton)
isotopeWeight In 113 = Just (112.904 *~ dalton)
isotopeWeight In 115 = Just (114.903 *~ dalton)
isotopeWeight Sn 112 = Just (111.904 *~ dalton)
isotopeWeight Sn 114 = Just (113.902 *~ dalton)
isotopeWeight Sn 115 = Just (114.903 *~ dalton)
isotopeWeight Sn 116 = Just (115.901 *~ dalton)
isotopeWeight Sn 117 = Just (116.902 *~ dalton)
isotopeWeight Sn 118 = Just (117.901 *~ dalton)
isotopeWeight Sn 119 = Just (118.903 *~ dalton)
isotopeWeight Sn 120 = Just (119.902 *~ dalton)
isotopeWeight Sn 122 = Just (121.903 *~ dalton)
isotopeWeight Sn 124 = Just (123.905 *~ dalton)
isotopeWeight Sb 121 = Just (120.903 *~ dalton)
isotopeWeight Sb 123 = Just (122.904 *~ dalton)
isotopeWeight Te 120 = Just (119.904 *~ dalton)
isotopeWeight Te 122 = Just (121.903 *~ dalton)
isotopeWeight Te 123 = Just (122.904 *~ dalton)
isotopeWeight Te 124 = Just (123.902 *~ dalton)
isotopeWeight Te 125 = Just (124.904 *~ dalton)
isotopeWeight Te 126 = Just (125.903 *~ dalton)
isotopeWeight Te 128 = Just (127.904 *~ dalton)
isotopeWeight Te 130 = Just (129.906 *~ dalton)
isotopeWeight I 127 = Just (126.904 *~ dalton)
isotopeWeight Xe 124 = Just (123.905 *~ dalton)
isotopeWeight Xe 126 = Just (125.904 *~ dalton)
isotopeWeight Xe 128 = Just (127.903 *~ dalton)
isotopeWeight Xe 129 = Just (128.904 *~ dalton)
isotopeWeight Xe 130 = Just (129.903 *~ dalton)
isotopeWeight Xe 131 = Just (130.905 *~ dalton)
isotopeWeight Xe 132 = Just (131.904 *~ dalton)
isotopeWeight Xe 134 = Just (133.905 *~ dalton)
isotopeWeight Xe 136 = Just (135.907 *~ dalton)
isotopeWeight Cs 133 = Just (132.905 *~ dalton)
isotopeWeight Ba 130 = Just (129.906 *~ dalton)
isotopeWeight Ba 132 = Just (131.905 *~ dalton)
isotopeWeight Ba 134 = Just (133.904 *~ dalton)
isotopeWeight Ba 135 = Just (134.905 *~ dalton)
isotopeWeight Ba 136 = Just (135.904 *~ dalton)
isotopeWeight Ba 137 = Just (136.905 *~ dalton)
isotopeWeight Ba 138 = Just (137.905 *~ dalton)
isotopeWeight La 138 = Just (137.907 *~ dalton)
isotopeWeight La 139 = Just (138.906 *~ dalton)
isotopeWeight Ce 136 = Just (135.907 *~ dalton)
isotopeWeight Ce 138 = Just (137.905 *~ dalton)
isotopeWeight Ce 140 = Just (139.905 *~ dalton)
isotopeWeight Ce 142 = Just (141.909 *~ dalton)
isotopeWeight Pr 141 = Just (140.907 *~ dalton)
isotopeWeight Nd 142 = Just (141.907 *~ dalton)
isotopeWeight Nd 143 = Just (142.909 *~ dalton)
isotopeWeight Nd 144 = Just (143.910 *~ dalton)
isotopeWeight Nd 145 = Just (144.912 *~ dalton)
isotopeWeight Nd 146 = Just (145.913 *~ dalton)
isotopeWeight Nd 148 = Just (147.916 *~ dalton)
isotopeWeight Nd 150 = Just (149.920 *~ dalton)
isotopeWeight Pm 145 = Just (144.912 *~ dalton)
isotopeWeight Pm 147 = Just (146.915 *~ dalton)
isotopeWeight Sm 144 = Just (143.912 *~ dalton)
isotopeWeight Sm 147 = Just (146.914 *~ dalton)
isotopeWeight Sm 148 = Just (147.914 *~ dalton)
isotopeWeight Sm 149 = Just (148.917 *~ dalton)
isotopeWeight Sm 150 = Just (149.917 *~ dalton)
isotopeWeight Sm 152 = Just (151.919 *~ dalton)
isotopeWeight Sm 154 = Just (153.922 *~ dalton)
isotopeWeight Eu 151 = Just (150.919 *~ dalton)
isotopeWeight Eu 153 = Just (152.921 *~ dalton)
isotopeWeight Gd 152 = Just (151.919 *~ dalton)
isotopeWeight Gd 154 = Just (153.920 *~ dalton)
isotopeWeight Gd 155 = Just (154.922 *~ dalton)
isotopeWeight Gd 156 = Just (155.922 *~ dalton)
isotopeWeight Gd 157 = Just (156.923 *~ dalton)
isotopeWeight Gd 158 = Just (157.924 *~ dalton)
isotopeWeight Gd 160 = Just (159.927 *~ dalton)
isotopeWeight Tb 159 = Just (158.925 *~ dalton)
isotopeWeight Dy 156 = Just (155.924 *~ dalton)
isotopeWeight Dy 158 = Just (157.924 *~ dalton)
isotopeWeight Dy 160 = Just (159.925 *~ dalton)
isotopeWeight Dy 161 = Just (160.926 *~ dalton)
isotopeWeight Dy 162 = Just (161.926 *~ dalton)
isotopeWeight Dy 163 = Just (162.928 *~ dalton)
isotopeWeight Dy 164 = Just (163.929 *~ dalton)
isotopeWeight Ho 165 = Just (164.930 *~ dalton)
isotopeWeight Er 162 = Just (161.928 *~ dalton)
isotopeWeight Er 164 = Just (163.929 *~ dalton)
isotopeWeight Er 166 = Just (165.930 *~ dalton)
isotopeWeight Er 167 = Just (166.932 *~ dalton)
isotopeWeight Er 168 = Just (167.932 *~ dalton)
isotopeWeight Er 170 = Just (169.935 *~ dalton)
isotopeWeight Tm 169 = Just (168.934 *~ dalton)
isotopeWeight Yb 168 = Just (167.933 *~ dalton)
isotopeWeight Yb 170 = Just (169.934 *~ dalton)
isotopeWeight Yb 171 = Just (170.936 *~ dalton)
isotopeWeight Yb 172 = Just (171.936 *~ dalton)
isotopeWeight Yb 173 = Just (172.938 *~ dalton)
isotopeWeight Yb 174 = Just (173.938 *~ dalton)
isotopeWeight Yb 176 = Just (175.942 *~ dalton)
isotopeWeight Lu 175 = Just (174.940 *~ dalton)
isotopeWeight Lu 176 = Just (175.942 *~ dalton)
isotopeWeight Hf 174 = Just (173.940 *~ dalton)
isotopeWeight Hf 176 = Just (175.941 *~ dalton)
isotopeWeight Hf 177 = Just (176.943 *~ dalton)
isotopeWeight Hf 178 = Just (177.943 *~ dalton)
isotopeWeight Hf 179 = Just (178.945 *~ dalton)
isotopeWeight Hf 180 = Just (179.946 *~ dalton)
isotopeWeight Ta 180 = Just (179.947 *~ dalton)
isotopeWeight Hf 181 = Just (180.947 *~ dalton)
isotopeWeight W 180 = Just (179.946 *~ dalton)
isotopeWeight W 182 = Just (181.948 *~ dalton)
isotopeWeight W 183 = Just (182.950 *~ dalton)
isotopeWeight W 184 = Just (183.950 *~ dalton)
isotopeWeight W 186 = Just (185.954 *~ dalton)
isotopeWeight Re 185 = Just (184.952 *~ dalton)
isotopeWeight Re 187 = Just (186.955 *~ dalton)
isotopeWeight Os 184 = Just (183.952 *~ dalton)
isotopeWeight Os 186 = Just (185.953 *~ dalton)
isotopeWeight Os 187 = Just (186.955 *~ dalton)
isotopeWeight Os 188 = Just (187.955 *~ dalton)
isotopeWeight Os 189 = Just (188.958 *~ dalton)
isotopeWeight Os 190 = Just (189.958 *~ dalton)
isotopeWeight Os 192 = Just (191.961 *~ dalton)
isotopeWeight Ir 191 = Just (190.960 *~ dalton)
isotopeWeight Ir 193 = Just (192.962 *~ dalton)
isotopeWeight Pt 190 = Just (189.959 *~ dalton)
isotopeWeight Pt 192 = Just (191.961 *~ dalton)
isotopeWeight Pt 194 = Just (193.962 *~ dalton)
isotopeWeight Pt 195 = Just (194.964 *~ dalton)
isotopeWeight Pt 196 = Just (195.964 *~ dalton)
isotopeWeight Pt 198 = Just (197.967 *~ dalton)
isotopeWeight Au 197 = Just (196.966 *~ dalton)
isotopeWeight Hg 196 = Just (195.965 *~ dalton)
isotopeWeight Hg 198 = Just (197.966 *~ dalton)
isotopeWeight Hg 199 = Just (198.968 *~ dalton)
isotopeWeight Hg 200 = Just (199.968 *~ dalton)
isotopeWeight Hg 201 = Just (200.970 *~ dalton)
isotopeWeight Hg 202 = Just (201.970 *~ dalton)
isotopeWeight Hg 204 = Just (203.973 *~ dalton)
isotopeWeight Tl 203 = Just (202.972 *~ dalton)
isotopeWeight Tl 205 = Just (204.974 *~ dalton)
isotopeWeight Pb 204 = Just (203.973 *~ dalton)
isotopeWeight Pd 206 = Just (205.974 *~ dalton)
isotopeWeight Pd 207 = Just (206.975 *~ dalton)
isotopeWeight Pd 208 = Just (207.976 *~ dalton)
isotopeWeight Bi 209 = Just (208.980 *~ dalton)
isotopeWeight Po 209 = Just (208.982 *~ dalton)
isotopeWeight Po 210 = Just (209.982 *~ dalton)
isotopeWeight At 210 = Just (209.987 *~ dalton)
isotopeWeight At 211 = Just (210.987 *~ dalton)
isotopeWeight Rn 211 = Just (210.990 *~ dalton)
isotopeWeight Rn 220 = Just (220.011 *~ dalton)
isotopeWeight Rn 222 = Just (222.017 *~ dalton)
isotopeWeight Fr 223 = Just (223.019 *~ dalton)
isotopeWeight Ra 223 = Just (223.018 *~ dalton)
isotopeWeight Ra 224 = Just (224.020 *~ dalton)
isotopeWeight Ra 226 = Just (226.025 *~ dalton)
isotopeWeight Ra 228 = Just (228.031 *~ dalton)
isotopeWeight Ac 227 = Just (227.027 *~ dalton)
isotopeWeight Th 230 = Just (230.033 *~ dalton)
isotopeWeight Th 232 = Just (232.038 *~ dalton)
isotopeWeight Pa 231 = Just (231.035 *~ dalton)
isotopeWeight U 233 = Just (233.039 *~ dalton)
isotopeWeight U 234 = Just (234.040 *~ dalton)
isotopeWeight U 235 = Just (235.043 *~ dalton)
isotopeWeight U 236 = Just (236.045 *~ dalton)
isotopeWeight U 238 = Just (238.050 *~ dalton)
isotopeWeight Np 236 = Just (236.046 *~ dalton)
isotopeWeight Np 237 = Just (237.048 *~ dalton)
isotopeWeight Pu 238 = Just (238.049 *~ dalton)
isotopeWeight Pu 239 = Just (239.052 *~ dalton)
isotopeWeight Pu 240 = Just (240.053 *~ dalton)
isotopeWeight Pu 241 = Just (241.056 *~ dalton)
isotopeWeight Pu 242 = Just (242.058 *~ dalton)
isotopeWeight Pu 244 = Just (244.064 *~ dalton)
isotopeWeight Am 241 = Just (241.056 *~ dalton)
isotopeWeight Am 243 = Just (243.061 *~ dalton)
isotopeWeight Cm 243 = Just (243.061 *~ dalton)
isotopeWeight Cm 244 = Just (244.062 *~ dalton)
isotopeWeight Cm 245 = Just (245.065 *~ dalton)
isotopeWeight Cm 246 = Just (246.067 *~ dalton)
isotopeWeight Cm 247 = Just (247.070 *~ dalton)
isotopeWeight Cm 248 = Just (248.072 *~ dalton)
isotopeWeight Bk 247 = Just (247.070 *~ dalton)
isotopeWeight Bk 249 = Just (249.074 *~ dalton)
isotopeWeight Cf 249 = Just (249.074 *~ dalton)
isotopeWeight Cf 250 = Just (250.076 *~ dalton)
isotopeWeight Cf 251 = Just (251.079 *~ dalton)
isotopeWeight Cf 252 = Just (252.081 *~ dalton)
isotopeWeight Es 252 = Just (252.082 *~ dalton)
isotopeWeight Fm 257 = Just (257.095 *~ dalton)
isotopeWeight Md 258 = Just (258.098 *~ dalton)
isotopeWeight Md 260 = Just (260.103 *~ dalton)
isotopeWeight No 259 = Just (259.101 *~ dalton)
isotopeWeight Lr 262 = Just (262.109 *~ dalton)
isotopeWeight Rf 267 = Just (267.121 *~ dalton)
isotopeWeight Db 268 = Just (268.125 *~ dalton)
isotopeWeight Sg 271 = Just (271.133 *~ dalton)
isotopeWeight Bh 272 = Just (272.138 *~ dalton)
isotopeWeight Hs 270 = Just (270.134 *~ dalton)
isotopeWeight Mt 276 = Just (276.151 *~ dalton)
isotopeWeight Ds 281 = Just (281.164 *~ dalton)
isotopeWeight Rg 280 = Just (280.165 *~ dalton)
isotopeWeight Cn 285 = Just (285.177 *~ dalton)
isotopeWeight Nh 284 = Just (284.178 *~ dalton)
isotopeWeight Fl 289 = Just (289.190 *~ dalton)
isotopeWeight Mc 288 = Just (288.192 *~ dalton)
isotopeWeight Lv 293 = Just (293.204 *~ dalton)
isotopeWeight Ts 292 = Just (292.207 *~ dalton)
isotopeWeight Og 294 = Just (294.213 *~ dalton)
isotopeWeight _ _ = Nothing

instance FormulaElement a => FormulaElement (Isotope a) where
    toFormulaPrec p (Isotope x n) = showParen' (p >= 6) ((asSup n <>) . toFormulaPrec 5 x)

instance Weight (Isotope Element) where
    weight (Isotope e n) = isotopeWeight e n

instance Arbitrary a => Arbitrary (Isotope a) where
    arbitrary = arbitrary1

instance Arbitrary1 Isotope where
    liftArbitrary arb = Isotope <$> arb <*> arbitrary
