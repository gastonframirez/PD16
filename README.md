# Trabajo Final: Programación Declarativa UBP '16

Juego de Cartas _Chinchón_ en **Haskell**

## Reglas del Juego: 

### Descripción
El Chinchón es un juego de baraja española de 40 o 48 cartas que se juega, a elección de los jugadores, con o sin comodines.
Toman parte individualmente dos o más jugadores.

  En la versión del Chinchón que aquí se ofrece el número de jugadores puede ser 2, 3 ó 4.

###Objetivo del juego
Al iniciar la partida se establece un número de puntos al que se jugará la misma. Los jugadores que sobrepasen estos puntos van siendo eliminados. Se juega a tantas manos o juegos parciales como sea necesario hasta que sólo quede un jugador tras eliminar a todos sus rivales.
El objetivo en cada una de las manos o repartos de cartas para cada jugador es sumar el menor número de puntos posible. Para ello deberá ligar sus cartas antes que sus rivales.

### Desarrollo del juego
#### Comienzo

Por sorteo se decide quién es el primero que reparte.
El jugador que reparte barajará el mazo y lo dará a cortar al que está situado a su izquierda, tras lo cual distribuirá siete cartas a cada jugador de una en una comenzando por el que está situado a su derecha y siguiendo en este mismo sentido.
Repartidas siete cartas a cada jugador, deberá descubrir la siguiente del mazo y dejarla visible en el centro del tapete, al lado del resto del mazo, que dejará boca abajo.

#### Jugar las cartas

Empezará realizando movimiento el mano (a la derecha del que reparte), que lo hará decidiendo si para su objetivo de ligar las cartas que tenga en la mano le interesa más tomar la carta descubierta del centro o tomar la carta cubierta de la parte superior del mazo.
Una vez tomada una de estas cartas (tras lo cual tendrá 8 cartas en la mano) deberá inmediatamente lanzar una de ellas, la que desee, descubierta, al centro del tapete, sobre las que pudiera haber allí descubiertas.
En los turnos sucesivos todos los jugadores deberán repetir este movimiento de tomar una de las cartas superiores, bien del mazo, cubiertas, o bien la carta superior del montón que se vaya formando, descubiertas, lanzando posteriormente una descubierta.
Si las cartas del mazo se agotan antes de finalizar la mano, se toman las cartas lanzadas y, una vez barajadas, se forma el mazo con ellas.

#### Ligar las cartas

A los efectos que se describen a continuación, el orden de las cartas de la baraja, de menor a mayor, en el Chinchón es el que sigue:

⋅⋅* Baraja de 40 cartas: 1, 2, 3, 4, 5, 6, 7, sota, caballo, rey.
⋅⋅* Baraja de 48 cartas: 1, 2, 3, 4, 5, 6, 7, 8, 9, sota, caballo, rey.
En cada mano, cada jugador tiene como objetivo intentar ligar sus cartas. Se entiende por ligar las cartas hacer grupos de al menos tres cartas unidas por uno de los criterios siguientes:

Cartas del mismo índice.
⋅⋅* Cartas que forman una escalera (un grupo de cartas del mismo palo con índices correlativos).
⋅⋅* Si la partida se crea para ser jugada con comodines (dos) éstos podrán sustituir, a gusto de quien los tenga, a cualquier carta de la baraja para intentar ligar las demás que tenga, debiendo en cada grupo de cartas haber por lo menos dos que no sean comodín.

#### Cerrar la mano
Un jugador en cualquiera de sus turnos, en el momento de deshacerse de una carta, puede cerrar la mano si tuviera todas las cartas ligadas (sea en dos grupos de tres y cuatro cartas o en un único grupo de 7 cartas) o si sólo le quedara una carta sin ligar y ésta tuviera un valor igual o inferior a 4 (el valor de las cartas es el de su índice salvo la sota, el caballo y el rey que valen 10 puntos).
Para cerrar la mano echará la carta de la que se va a deshacer, sobre el resto de cartas desechadas, pero vuelta hacia abajo, y seguidamente mostrará su jugada.
  En Ludoteka para cerrar la mano debe pulsarse sobre la imagen del candado antes de lanzar la carta.
En la primera vuelta de cada mano no se puede cerrar.
En determinados casos, cuando los jugadores se encuentran a la espera de cartas que están en manos de los rivales, la mano puede prolongarse por excesivo tiempo; en estos casos, el cierre se produce de manera automática cuando las cartas del mazo se agotan por cuarta vez.

#### Colocar las cartas

En el Chinchón, una vez que la mano ha sido cerrada por un jugador, el objetivo de los demás es intentar sumar la menor cantidad posible de puntos. Para ello, los jugadores, empezando por el que cierra y en el mismo orden en que tenían el turno de juego, van mostrando los grupos de cartas que pudieran tener ligadas.

Una vez mostradas todas las cartas ligadas, el jugador colocará aquellas que pudiera tener que ligaran con los grupos previamente expuestos por los rivales. Esto puede realizarse tan sólo si quien cerró la mano lo hizo quedándose a su vez con alguna carta sin ligar.

Siempre que el jugador que cierra ha conseguido ligar sus 7 cartas, el proceso de mostrar las cartas lo realiza directamente el programa. En caso de tener seleccionada la automatización de movimientos forzados (), también lo hace aunque no todas las cartas se hayan ligado. Entre las diferentes opciones o combinaciones que pudieran ligarse, el programa escoge aquella que permite obtener la menor puntuación posible en función de las propias cartas y las mostradas por los rivales hasta ese momento. Este proceso acelera la dinámica del juego y en la gran mayoría de las ocasiones su resultado coincide con el que elegiría el propio jugador.
No obstante, el jugador puede prescindir de esa herramienta y escoger otra combinación en caso de querer guardarse una o varias cartas diferentes, bien por no favorecer con ello a otros jugadores, o bien por reservárselas para colocar en posibles grupos de cartas aún no mostrados. En este caso:

Las cartas ligadas en mano deben sacarse antes que las cartas a colocar en grupos mostrados por otros jugadores.
Las cartas deben ir marcándose una a una en el orden de salida (en el caso de las escaleras, de menor a mayor); las cartas del grupo que está siendo mostrado quedan marcadas.
Si la siguiente carta a mostrar es obligada, el propio programa lo hace directamente.
Cuando el jugador da por acabada la fase de sacar un grupo de cartas, debe pulsar el icono situado a la derecha (), salvo que no sea posible ubicar más cartas en el grupo.
Al colocar cartas en grupos mostrados previamente, si la carta elegida puede ubicarse en diferentes lugares, ésta queda marcada y es preciso hacer click a continuación sobre la carta junto a la que prefiere situarse.
Cuando el jugador dispone de alguna carta para ubicar en un grupo previamente mostrado, pero sin embargo da por acabada la fase de sacar cartas, debe pulsar el icono situado a la derecha ().

#### Recuento de la mano
Si quien cerró tenía todas sus cartas ligadas obtendrá una puntuación negativa en base a las siguientes reglas:

Con dos grupos de cartas (uno de tres y otro de cuatro) será -10.
Con una escalera de siete cartas con dos comodines será -25.
Con una escalera de siete cartas con un comodín será -50.
Con una escalera de siete cartas sin comodines (chinchón) ganará la partida directamente.
Una vez colocadas todas las cartas posibles por parte de los jugadores, cada cual sumará los puntos de aquellas que siguiera teniendo sin colocar, pasando a aumentar estos puntos el tanteo total de la partida.

#### Eliminaciones y reenganches
Cuando un jugador supera el número de puntos previamente establecido como límite de la partida, es eliminado de la misma. Si en el momento de abrir la partida, la misma se hubiera configurado con un determinado número de reenganches, en el momento de ser eliminado, cada jugador podrá reincorporarse a la partida tantas veces como se hubiera determinado, haciéndolo con la puntuación de aquel jugador que tuviera la máxima puntuación.
Si un jugador logra que todos los rivales sobrepasen la puntuación establecida en una mano, gana aunque el resto no haya gastado sus reenganches.
Cuando un jugador se encuentra cercano al límite de la puntuación de eliminación, no puede cerrar manteniendo en la mano una carta con la que sobrepase dicha puntuación.
En caso de partidas entre dos jugadores no son posibles los reenganches.

#### Final de partida
En el Chinchón vence la partida aquel jugador que hubiera conseguido eliminar a todos los demás jugadores.





