module Functions.Matriz (ejercito,calculaEjercito)where
import Data.Matrix as M
import System.Directory

------------------------------------------------------------
-- Ejercito:  Matriz de 1's, luego calculaEjercito calcula
-- el número de filas y columnas y los multiplica para saber
-- el número de 1's que hay en total.
------------------------------------------------------------
ejercito :: (Num a, Enum a) => Matrix a
ejercito = M.fromLists [[1,1,1,1,1],
                        [1,1,1,1,1],
                        [1,1,1,1,1],
                        [1,1,1,1,1],
                        [1,1,1,1,1]]

calculaEjercito :: Matrix a -> Int 
calculaEjercito m = (nrows m) * (ncols m)

    

        