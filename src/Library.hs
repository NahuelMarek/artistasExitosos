module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero


type Cancion = String

data Artista = UnArtista {
    nombre :: String,
    canciones :: [Cancion]
} deriving Show

fitito :: Artista
fitito = UnArtista "Fitito Paez" ["11 y 6", "El amor despues del amor", "Mariposa Tecknicolor"]

calamardo :: Artista
calamardo = UnArtista "Andres Calamardo" ["Flaca", "Sin Documentos", "Tuyo siempre"]

paty :: Artista
paty = UnArtista "Taylor Paty" ["Shake It Off", "Lover"]

grupoDeArtistas :: [Artista] 
grupoDeArtistas = [fitito, paty, calamardo]

--punto 1
calificacionDeCancion :: Cancion -> Number
calificacionDeCancion = (10+).length.soloMinusculas

soloMinusculas :: Cancion->Cancion
soloMinusculas cancion = filter letraEsMinuscula cancion

letraEsMinuscula :: Char -> Bool
letraEsMinuscula letra = elem letra "abcdefghijklmnopqrstuvwxyz"

--------------------------- otra forma ----------------------------------------------------
calificacionDeCancion' :: Cancion -> Number
calificacionDeCancion' = (10+).length.filter (\cancion -> elem cancion "abcdefghijklmnopqrstuvwxyz")

-----------------------------------------------------------------------------------------
--punto 2
esExitoso :: Artista -> Bool
esExitoso = (>50).sum.cancionesBuenas.canciones

cancionesBuenas :: [Cancion] -> [Number] 
cancionesBuenas = filter (>20).map calificacionDeCancion

----------- otra forma ---------------------
esExitoso' :: Artista -> Bool
esExitoso' = (>50).sum.filter (>20).map calificacionDeCancion.canciones

-----------------------------------------------------------------------------------------
--punto 3
artistasExitosos :: [Artista] -> [String]
artistasExitosos = map (\artista -> nombre artista).(filter esExitoso)
  
--punto 4 ????
artistasExitosos' :: [Artista] -> [String]  
artistasExitosos' = map (\artista -> nombre artista).filter ((>50).sum.filter (>20).map ((10+).length.filter (\cancion -> elem cancion "abcdefghijklmnopqrstuvwxyz")).canciones)