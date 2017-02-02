import Chinchon
import System.Random
import System.IO
import Control.Monad 

main :: IO ()
main = do
	hSetBuffering stdin LineBuffering
	initialState <- setup
	jugar initialState 1 0


setup :: IO (EstadoDeJuego)
setup = do
	-- shuffle cards
	mazo <- shuffleIO nuevoMazo
	-- create player
	putStrLn "Cual es tu nombre?"
	player <- nuevoJugador <$> getLine
	
	let computadora = nuevoJugador "Computadora"
	-- deal 7 cards to all players
	let (mazo', player') = repartirNCartasAJugador 7 mazo player
	let player'' = (Jugador (nombre player') (ordenarTodasCartas (mano player')) [] [])
	let (mazo'', computadora') = repartirNCartasAJugador 7 mazo' computadora
	let computadora''  = configurarComputadora computadora'
	-- turn 1 card and make a discarded pile
	let (cartaDescartada, mazoAux) = repartir mazo''
	let pilaDescartada = [cartaDescartada]

	return (player'', computadora'', mazoAux, pilaDescartada)

esPar :: Int -> Bool
esPar 0 = True
esPar n = n `rem` 2 == 0

jugar :: EstadoDeJuego -> Int -> Int-> IO ()
jugar estado@(jugador, computadora, mazo, pilaDescartadas) turno corto =
	if length mazo == 0
	then do	
		let pilaDescartadas' = head pilaDescartadas
		mazo' <- shuffleIO (tail pilaDescartadas)
		jugar (jugador, computadora, mazo', [pilaDescartadas']) turno corto
	else
		if corto == 1
			then if esChinchon (mano jugador)
				then putStrLn $ "Ganaste el juego! " ++ mostrarMano estado
			else
				if puedeGanarMenosDiez (mano jugador)
					then putStrLn $ "Ganaste la ronda con menos 10! " ++ mostrarMano estado
				else 
					if puedeGanarSobraUna (mano jugador)
						then  putStrLn $ "Ganaste la ronda! Sumas:  " ++ mostrarMano estado
					else do
					putStrLn $ "No podés cortar todavia! Intentá cuando tengas combinaciones.\n"
					jugar estado (turno) 0
			
		else if corto == 2 
			then if esChinchon (mano computadora)
				then putStrLn $ "Computadora gano el juego! " ++ mostrarManoCompu estado
			else
				if puedeGanarMenosDiez (mano computadora)
					then putStrLn $ "Computadora gano la ronda, con menos 10! " ++ mostrarManoCompu estado
				else 
					if puedeGanarSobraUnaComputadora (mano computadora)
						then  putStrLn $ "Computadora gano la ronda, suma: " ++ mostrarManoCompu estado
					else jugar estado (turno) 0
			
		else
			if not (esPar turno)
			then do
				putStrLn $ "\nEstado Compu Despues:" ++ show computadora
				putStrLn $ "\nDescartadas por Compu:" ++ show pilaDescartadas
				estado' <- levantarCarta estado
				putStrLn $ mostrarMano estado'
				putStrLn "Que querés hacer ahora? \n  1) Tirar carta\n  2) Tirar y cortar"
				respuesta <- readLn
				estado'' <- descartarCarta estado'
				putStrLn $ mostrarMano estado''
				if respuesta == 2
					then jugar estado'' (turno) 1
				else
					jugar estado'' (turno+1) 0
			else do
				putStrLn $ "\nDescartadas por user:" ++ show pilaDescartadas
				putStrLn $ "Primera mazo: " ++ show (head mazo)
				putStrLn $ "\nEstado Compu:" ++ show computadora
				putStrLn $ "Mano Compu Antes: " ++ mostrarManoCompu estado
				let estado' = jugadaComputadora estado
				let computadoraDespues = devolverComputadoraEstado estado'
				if puedeGanarComputadora computadoraDespues
				then  jugar estado' (turno) 2
				else jugar estado' (turno+1) 0


levantarCarta :: EstadoDeJuego -> IO EstadoDeJuego
levantarCarta estado = do
	putStrLn $ "\nTu mano: " ++ mostrarMano estado
	putStrLn $ "Que te gustaría hacer ahora?\n  1) tomar una carta desconocida\n  2) tomar la ultima carta descartada (" ++ (mostrarUltimaCartaDescartada estado) ++ ")"
	respuesta <- readLn
	return $ (if respuesta == 1 then tomarCartaDesconocida estado else tomarUltimaCartaDescartada estado)


descartarCarta :: EstadoDeJuego -> IO EstadoDeJuego
descartarCarta ((Jugador nombre mano s ss), computadora, mazoRestante, pilaDescartadas) = do
	putStrLn $ "\nTu mano: " ++ show mano
	putStrLn "Elegí una carta a descartar (1-8)"
	n <- readLn
	-- VER COMO HACER PARA QUE NO TOME VALORES MENORES a 1 y MAYORES a 8
	let (mano', pilaDescartadas') = descartar mano n pilaDescartadas
	return ((Jugador nombre mano' s ss), computadora, mazoRestante, pilaDescartadas')
