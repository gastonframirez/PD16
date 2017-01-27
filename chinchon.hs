module Chinchon where

import Data.List

--Creacion mazo de 52 cartas de poker. Cada palo esta formado por 13 cartas, de las cuales 9 cartas son numerales y 4 literales. 
--Se ordenan de menor a mayor rango de la siguiente forma: A, 2, 3, 4, 5, 6, 7, 8, 9, 10, J, Q y K 

data Palo =  Treboles | Diamantes | Corazones | Picas deriving (Eq, Ord, Enum)
data Valor = As | Dos | Tres | Cuatro | Cinco | Seis | Siete | Ocho | Nueve | Diez | Jack | Reina | Rey deriving (Show, Eq, Ord, Enum)
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
    show (Carta As palo)   = "A" ++ show palo
    show (Carta Dos palo)   = "2" ++ show palo
    show (Carta Tres palo) = "3" ++ show palo
    show (Carta Cuatro palo)  = "4" ++ show palo
    show (Carta Cinco palo)  = "5" ++ show palo
    show (Carta Seis palo)   = "6" ++ show palo
    show (Carta Siete palo) = "7" ++ show palo
    show (Carta Ocho palo) = "8" ++ show palo
    show (Carta Nueve palo)  = "9" ++ show palo
    show (Carta Diez palo)   = "10" ++ show palo
    show (Carta Jack palo)  = "J" ++ show palo
    show (Carta Reina palo) = "Q" ++ show palo
    show (Carta Rey palo)  = "K" ++ show palo

nuevoMazo :: Mazo
nuevoMazo = [Carta p n | n <- [Treboles .. ], p <- [As ..]]

-- Hacer funcion para mezclar cartas

-- Hacer funcion para repartir cartas