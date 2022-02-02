module Functions.Bufon(adivinanzaF,chistes,cuentaChiste,cuentaChisteCorto,cuentaChisteLargo) where

-- ---------------------------------------------------------------------
-- Adivinanza del Bufón: según lo que hayamos indicado por entrada de
-- teclado nos dirá si la respuesta es correcta o no.
-- ---------------------------------------------------------------------
adivinanzaF :: Int -> String
adivinanzaF n ="Respuesta " ++
  case n of
    1 -> "incorrecta"
    n
      | n==2 -> "correcta"
      | n==3 -> "incorrecta"

-- ---------------------------------------------------------------------
-- Chistes del Bufón: según si le hemos pedido un chiste largo o uno
-- corto, nos devolverá el String  más largo o el corto.
-- ---------------------------------------------------------------------
chistes :: [(String)]
chistes = [("¿Cuál es el último animal que subió al famoso arca de Noe? El del-fin."), ("Van 2 y se cae el del medio.")]

cuentaChiste :: Int -> [(String)]
cuentaChiste chiste
    | chiste==1 = cuentaChisteCorto
    | chiste==2 = cuentaChisteLargo

cuentaChisteCorto :: [(String)]
cuentaChisteCorto = [x | x <- chistes, length x == m]
  where m = minimum [length x | x <- chistes]

cuentaChisteLargo :: [(String)]
cuentaChisteLargo = [x | x <- chistes, length x == m]
  where m = maximum [length x | x <- chistes]
