module Functions.TipoAlgebraico(Objeto,tirachinas,picotienda,pocion,nombre,descripcion,precio,
                      Persona,nombrePersona,altura,peso,edad,personaElegida) where

-- ---------------------------------------------------------------------
-- Tipos de datos algebráicos
-- ---------------------------------------------------------------------

-- Tipo objeto, lo usaremos para los objetos de la tienda
data Objeto = Objeto {nombre :: String, descripcion :: String, precio :: Int} deriving (Show,Eq)

tirachinas :: Objeto 
tirachinas = Objeto {nombre="Tirachinas",descripcion="Un pequeño tirachinas viejo... quizás te sirva para cazar algún conejo.",precio= 5}

picotienda :: Objeto 
picotienda = Objeto {nombre="Pico",descripcion="Muy afilado y de calidad para minar.",precio= 20}

pocion :: Objeto 
pocion = Objeto {nombre="Poción de Salud",descripcion="¿Para qué quieres una poción si en este juego no te puedes morir?",precio= 50}

--Tipo persona, lo usaremos para leer el censo de la ciudad
data Persona = Persona {nombrePersona :: String, altura :: Int, peso :: Int, edad :: Int} deriving (Show,Eq)

guardia :: Persona
guardia = Persona {nombrePersona="Gerardo",altura=190,peso=80,edad=40}

frutero :: Persona
frutero = Persona {nombrePersona="Manolo",altura=160,peso=90,edad=65}

giganteP :: Persona
giganteP = Persona {nombrePersona="Goliat",altura=1000,peso=1500,edad=200}

tendero :: Persona
tendero = Persona {nombrePersona="Gonzalo",altura=170,peso=60,edad=55}

bufon :: Persona
bufon = Persona {nombrePersona="Joaquin",altura=180,peso=120,edad=45}

personaElegida :: [Char] -> Persona
personaElegida n
    | n == "1" = guardia
    | n == "2" = frutero
    | n == "3" = giganteP
    | n == "4" = tendero
    | n == "5" = bufon