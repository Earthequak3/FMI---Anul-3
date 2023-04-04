{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s i ) = s `elem` ["Sanguinello","Tarocco","Moro"]
ePortocalaDeSicilia  (Mar s b) = False

getInt :: Fruct -> Int
getInt (Portocala s i ) = i

test_ePortocalaDeSicilia1 :: Bool
test_ePortocalaDeSicilia1 =
    ePortocalaDeSicilia (Portocala "Moro" 12) == True

test_ePortocalaDeSicilia2 :: Bool
test_ePortocalaDeSicilia2 =
    ePortocalaDeSicilia (Mar "Ionatan" True) == False

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia [] = 0
nrFeliiSicilia (x:xs) 
                        | ePortocalaDeSicilia x = i + nrFeliiSicilia xs
                        | otherwise = nrFeliiSicilia xs
                        where i = getInt x

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

nrMereViermi :: [Fruct] -> Int
nrMereViermi [] = 0
nrMereViermi (x:xs) 
                    | marVierme x = 1 + nrMereViermi xs
                    | otherwise = nrMereViermi xs
                        where marVierme (Mar s b) = b
                              marVierme (Portocala _ _ ) = False

test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica a)= "Meow"
vorbeste (Caine a b) = "Woof"

getRasa :: Animal -> Rasa
getRasa (Caine n r) = r

rasa :: Animal -> Maybe String
rasa a 
        | vorbeste a == "Meow" = Nothing
        | vorbeste a == "Woof" = Just r
        where r = getRasa a
             


data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

verifica :: Matrice -> Int -> Bool
verifica (M matrice) n = foldr ( (&&) . verificaSumaLinie n ) True matrice
                            where verificaSumaLinie n (L linie) = sum linie == n

test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True
doarPozN :: Matrice -> Int -> Bool
doarPozN (M liniile) n = all (\(L linie) -> all (> 0)  linie) (filter (\(L linie) -> length linie == n) liniile)

testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True

testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False
corect :: Matrice -> Bool
corect (M matrice)= all (\linie -> length linie == length ( head matrice)) matrice

testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True
