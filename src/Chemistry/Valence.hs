module Chemistry.Valence where

import Chemistry.Element(Element(B, C, N, O, P, S, F, Cl, Br, I, At))

smilesValence :: Element -> [Int]
smilesValence B = [3]
smilesValence C = [4]
smilesValence N = [3,5]
smilesValence O = [2]
smilesValence P = [3,5]
smilesValence S = [2,4,6]
smilesValence F = [1]
smilesValence Cl = [1]
smilesValence Br = [1]
smilesValence I = [1]
smilesValence At = [1]
smilesValence _ = []
