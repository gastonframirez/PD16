module Chinchon where

import Data.List

--Creacion mazo de 48 cartas con simbolos de poker pero sin los 4 literales (A, J , Q, K) 
--Se ordenan de menor a mayor rango de la siguiente forma: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11 y 12 

data Palo =  Treboles | Diamantes | Corazones | Picas deriving (Eq, Ord, Enum)
data Valor = Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve | Diez | Once | Doce deriving (Show, Eq, Ord, Enum)
data Carta = Carta Valor Palo deriving (Eq)

type Mazo = [Carta]

--Se asignan simbolo ASCII a cada palo 
instance Show Palo where
    show Picas = "♠"
    show Corazones = "♥"
    show Diamantes = "♦"
    show Treboles = "♣"

--Se asignan numeros, literales y signos a cada carta depende su valor
instance Show Carta where
    show (Carta Uno palo)   = "1" ++ show palo
    show (Carta Dos palo)   = "2" ++ show palo
    show (Carta Tres palo) = "3" ++ show palo
    show (Carta Cuatro palo)  = "4" ++ show palo
    show (Carta Cinco palo)  = "5" ++ show palo
    show (Carta Seis palo)   = "6" ++ show palo
    show (Carta Siete palo) = "7" ++ show palo
    show (Carta Ocho palo) = "8" ++ show palo
    show (Carta Nueve palo)  = "9" ++ show palo
    show (Carta Diez palo)   = "10" ++ show palo
    show (Carta Once palo)  = "11" ++ show palo
    show (Carta Doce palo) = "12" ++ show palo

