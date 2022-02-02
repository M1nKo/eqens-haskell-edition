module Functions.Tienda(colaTienda,vaciarCola,itemsTienda,descripcionItems) where
import Functions.ColaConListas
-- ---------------------------------------------------------------------
-- Cola Tienda
-- ---------------------------------------------------------------------
-- Colas Posibles:
c1 = foldr inserta vacia ["Guardia"]
c2 = foldr inserta vacia ["Frutero malhumorado","Guardia"]
c3 = foldr inserta vacia ["Guardia","Gigante","Mono de 3 cabezas"]
c4 = foldr inserta vacia ["Aldeano humilde","Guardia","Alquimista","Trovador"]

-- Cola elegida (Le pasamos como parametro un numero aleatorio entre 1-4):
colaTienda :: Int -> Cola [Char]
colaTienda numAleatorio
  | numAleatorio == 1 = c1
  | numAleatorio == 2 = c2
  | numAleatorio == 3 = c3
  | otherwise = c4

-- Le pasamos el numeroAleatorio(1-4), la cola elegida y []
vaciarCola :: Int -> Cola[Char] ->[Cola[Char]] -> [Cola [Char]]
vaciarCola 0 lista = lista
vaciarCola n lista = vaciarCola (n-1) (resto c) lista++[c]
  | n == 0 = lista
  | n /= 0 = vaciarCola (n-1) (resto c) lista++[c]
-- ---------------------------------------------------------------------
-- Descripción Items de la tienda
-- ---------------------------------------------------------------------

itemsTienda :: [(Int, String)]
itemsTienda = [(1, "Muy afilado y de calidad para minar"), (2, "Un pequeño tirachinas viejo... quizás te sirva para cazar algún conejo,"),
          (3, "¿Para qué quieres una poción si en este juego no te puedes morir?")]

descripcionItems :: Int -> [(String)]
descripcionItems n = [ s | (x, s) <- itemsTienda, x==n]