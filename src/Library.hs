module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Cancion = UnaCancion {
    titulo :: String,
    duracion :: Number,
    instrumentos :: [Instrumento]
} deriving (Show,Eq)

{-instancio canciones
Cada canción cuenta con un título, una duración en minutos y 
una lista de instrumentos musicales que se utilizarán en la interpretación.-}

patternMatching :: Cancion
patternMatching = UnaCancion "Pattern Matching" 4 [Guitarra, Bajo, Bateria]

seisDieciocho :: Cancion
seisDieciocho = UnaCancion "Seis dieciocho" 3 [Teclado, Guitarra]

laVidaEnHaskell :: Cancion
laVidaEnHaskell = UnaCancion "La vida en Haskell" 5 []

--Multiples Constructores de  instrumentos

data Instrumento = Guitarra | Bajo | Bateria | Teclado | Saxofon deriving (Show, Eq)

--funciones

aceptacion :: Cancion -> Number
aceptacion cancion
    | head (titulo cancion) == 'M' = 500   --head: Primer elemento
    | even (duracion cancion) = length (titulo cancion) * 10  --even: Par length:Longitud 
    | esAcapella (cancion) = 10 
    | otherwise = 0 

type Repertorio = [Cancion] 

repertorio :: Repertorio
repertorio = [patternMatching, seisDieciocho, laVidaEnHaskell, melodiasFuncionales, haskellEsAmor]

-- SE PIDE --
-- 1) Definir al menos 2 canciones más para la banda y agregarlas al repertorio.
melodiasFuncionales :: Cancion
melodiasFuncionales = UnaCancion "Melodias Funcionales" 2 [Guitarra]

haskellEsAmor :: Cancion
haskellEsAmor = UnaCancion "Haskell es amor" 6 []

-- 2) PdePop tiene la costumbre de tocar sus canciones por orden alfabético.
--    Dadas dos canciones, determinar cuál viene antes en el repertorio.
vieneAntes :: Cancion -> Cancion -> Cancion
vieneAntes cancion1 cancion2
    | titulo cancion1 < titulo cancion2 = cancion1
    | otherwise = cancion2

-- 3) Determinar si una canción es acapella.
esAcapella :: Cancion -> Bool
esAcapella cancion = null (instrumentos cancion) --Si está vacía

--4)Averiguar si una canción es aceptada por el público (aceptacion > 60)
esAceptada :: Cancion -> Bool
esAceptada cancion = aceptacion cancion > 60

-- 5) Dado un instrumento y una canción, determinar si la canción necesita al instrumento para ser interpretada. 
llevaInstrumento :: Instrumento -> Cancion -> Bool
llevaInstrumento _ (UnaCancion _ _ []) = False
llevaInstrumento instrumento cancion = elem instrumento (instrumentos cancion)


-- 6) Tocar una canción, esto implica que, si la canción es aceptada por el público, se la toca tal cual es,
--    en caso contrario, se la toca con la duración reducida a la mitad.
tocar :: Cancion -> Cancion
tocar cancion
    | esAceptada cancion = cancion
    | otherwise = cancion {duracion = duracion cancion / 2}