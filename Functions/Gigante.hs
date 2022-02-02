-- ---------------------------------------------------------------------
-- Funciones Secundarias
-- ---------------------------------------------------------------------
module Functions.Gigante
(alturaGigante,alturaConversion) where

-- ---------------------------------------------------------------------
-- Altura Gigante
-- ---------------------------------------------------------------------
-- Debido a que lo que recibimos por un getLine es un String y no se puede
-- operar con él, con la función "alturaConversión" devolvemos un Integer
-- con el mismo valor que introdujimos por el getLine, y ese Integer se lo
-- manda a la función alturaGigante que ya trabaja con el dato como un 
-- Integer. La función alturaGigante simplemente nos dice "cuantos jugadores"
-- equivalen a la altura del gigante, teniendo en cuenta que el gigante
-- mide 10m (1000cm)
 -- ---------------------------------------------------------------------

alturaGigante :: Integer -> Integer -> Integer -> Integer 
-- parámetros que introducir: AlturaDelJugador, 0, 0
alturaGigante alturaReal acum individuos
    | acum < 1000  = alturaGigante alturaReal (acum+alturaReal) (individuos+1)
    | acum > 1000  = individuos-1
    | acum == 1000 = individuos
  
-- parámetros que introducir: alturaDelJugador, 0
alturaConversion :: String -> Integer -> Integer
alturaConversion alturaReal n
  | show (n) == alturaReal = alturaGigante n 0 0
  | otherwise = alturaConversion alturaReal (n+1)







