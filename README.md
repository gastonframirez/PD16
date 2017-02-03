# Trabajo Final: Programación Declarativa UBP '16

Juego de Cartas _Chinchón_ en **Haskell*

## Descripción del Proyecto 

Para este trabajo final decidimos desarrollar un juego de cartas llamado Chinchón en el lenguaje de programación **Haskell**.
Para el desarrollo del juego, decidimos incluir una IA básica para que el usuario pueda jugar contra la computadora.

### Reglas del Juego:

#### Descripción
Nuestro Chinchón es un juego que usa la baraja de poker modificada (A = 1, 2, 3, 4, 5, 6, 7, 8, 9, J = 10, Q = 11, K = 12)(♠, ♥, ♦, ♣) de 48 cartas (sin comodines).
Toman parte individualmente dos jugadores (un usuario y la computadora).

#### Objetivo del juego
Cada partida se jugará hasta que alguno de los dos jugadores sobrepase los 100 puntos. El jugador que sobrepase estos puntos pierde el juego. Se juegan tantas manos como sean necesarias hasta que alguno pierda.
El objetivo para cada jugador en cada una de las rondas es sumar el menor número de puntos posible. Para ello deberá combinar sus cartas antes que su rival.

#### Desarrollo del juego
##### Comienzo

Automaticamente se mezcla el mazo y se reparten 7 cartas a cada jugador, y se deja visible la siguiente carta del mazo en un montón nuevo. El resto del mazo no es visible para ninguno de los dos jugadores. 
El usuario empieza la partida.

##### Jugar las cartas

El jugador tiene la posibilidad de levantar la carta visible o sacar la primera del mazo y esto dependerá de lo que quiera combinar.
Una vez tomada una de estas cartas (tras lo cual tendrá 8 cartas en la mano) deberá inmediatamente decidir de cual de ellas deshacerse, agregandola encima del montón de cartas visibles.
En los turnos sucesivos los jugadores deberán repetir este movimiento de tomar una de las cartas superiores, bien del mazo, o bien la carta superior del montón de cartas visibles.
Si las cartas del mazo se agotan antes de finalizar la mano, se toman las cartas visibles (excepto la superior, que se mantiene), se las mezcla y se forma un nuevo mazo con ellas.

##### Ligar las cartas

En cada mano, cada jugador tiene como objetivo intentar combinar sus cartas formando grupos de al menos tres cartas unidas por uno de los siguientes criterios:

  * Cartas del mismo número.
  * Cartas que forman una escalera (un grupo de cartas del mismo palo con nùmeros correlativos).

##### Cortar

Un jugador en cualquiera de sus turnos, en el momento de deshacerse de una carta, puede cortar si tuviera todas las cartas combinadas (sea en dos grupos, de tres y cuatro cartas, o en un único grupo de 7 cartas) o si sólo le quedara una carta sin combinar.

##### Recuento de la mano
El puntaje que obtendrá cada jugador cuando alguien corte se basa a las siguientes reglas:

  1. Si el jugador que cortó tiene una escalera de siete cartas (chinchón), ganará el juego directamente.
  2. Si el jugador tiene dos grupos de cartas (uno de tres y otro de cuatro):
    * Si el jugador es el que cortó, este obtendrá -10 puntos.
    * Sino, obtendrá 0 puntos.
  3. Si el jugador tiene combinaciones de 3 o 4 cartas, estas no se suman al puntaje de la ronda, que será la suma de las cartas que no se encuentarn combinadas.




