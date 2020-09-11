--version X
{-# LANGUAGE BlockArguments #-}
import Data.List(nub)

type Pos = (Int,Int)
irA ::  Pos-> IO()
irA (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

escribeEn :: Pos-> String->IO()
escribeEn p xs = do irA p
                putStr xs

limpiaPantalla:: IO ()
limpiaPantalla=putStr "\ESC[2J"

type Tablero = [Pos]

ancho :: Int
ancho = 5

alto :: Int
alto = 5

ejTablero :: Tablero
ejTablero = [(2,3),(3,4),(4,2),(4,3),(4,4)]

vida :: Int->Tablero-> IO()
vida n t=do limpiaPantalla
    escribeTablero t
    espera n
    vida n (siguienteGeneracion t)

escribeTablero :: Tablero-> IO()
escribeTablero t=sequence_[escribeEn p "O" | p <-t]

espera :: Int->IO()
espera n = sequence_ [return() | _<-[1..n]]

siguienteGeneracion :: Tablero -> Tablero
siguienteGeneracion t=supervivientes t ++ nacimientos t

supervivientes :: Tablero->[Pos]
supervivientes t = [p | p <-t, elem (nVecinosVivos t p)[2,3]]

nVecinosVivos :: Tablero -> Pos -> Int
nVecinosVivos t = length . filter (tieneVida t) . vecinos 

vecinos :: Pos -> [Pos]
vecinos (x,y) = map modular [(x-1,y-1), (x,y-1), (x+1,y-1), (x-1,y), (x+1,y), (x-1,y+1), (x,y+1), (x+1,y+1)]

modular :: Pos -> Pos
modular (x,y)=(1+ (x-1) + `mod` ancho, 1 + (y-1)`mod`alto)

tieneVida :: Tablero -> Pos -> Bool
tieneVida t p = elem p t

noTieneVida :: Tablero -> Pos -> Bool
noTieneVida t p = not (tieneVida t p)

nacimientos' :: Tablero -> [Pos]
nacimientos' t = [(x,y) | x <- [1..ancho], y <- [1..alto], noTieneVida t(x,y), nVecinosVivos t(x,y) == 3]

nacimientos :: Tablero -> [Pos]
nacimientos t = [p | p <- nub (concatMap vecinos t), noTieneVida t p, nVecinosVivos t p == 3]