import Text.Show.Functions

type Dia= Int
type Mes= Int
type Anio= Int
type Hobbie= Persona->Persona   -- puede pertenecer al data persona que es donde esta adentro

data Persona= Persona{
nombre::String,
fechaDeNac::(Dia,Mes,Anio),
gradoDeDiversion::Int,
hobbies::[Hobbie]
} deriving (Show)


mariano = Persona{
nombre = "Mariano",
fechaDeNac = (14,5,1986),
gradoDeDiversion=200,
hobbies=[videoJuegos 10, jardineria "ornamental",golf]
}




videoJuegos::Int->Hobbie
videoJuegos nivel persona = subirDiversion persona nivel  
--videoJuegos nivel persona = persona{gradoDeDiversion = gradoDeDiversion persona + aumentarDiversion nivel} 

aumentarDiversion::Int->Int
aumentarDiversion nivel = min 8 (div nivel 2) 

jardineria::String->Hobbie
jardineria tipo persona
 |tipo == "ornamental" = subirDiversion persona 10
 |tipo == "bonsai"     = subirDiversion persona 20
 |otherwise = subirDiversion persona (length tipo)

subirDiversion persona cant = persona{ gradoDeDiversion = gradoDeDiversion persona + cant}  
 
anio (_,_,a)= a

esCentennial::Persona->Bool
esCentennial = (>=1995).anio.fechaDeNac

{-
golf::Hobbie
golf persona 
 |take 3 (nombre persona) == "Sir" = persona{nombre= nombre persona}
 |otherwise= persona{nombre="Sir " ++ nombre persona}
-}

golf::Hobbie
golf persona = persona{nombre = agregarApodo (nombre persona)}  
 
agregarApodo::String->String
agregarApodo nombre 
 |take 3 nombre == "Sir" = nombre
 |otherwise = "Sir " ++ nombre  
 
{-
Algunas pruebas posibles
A continuación se muestran algunas pruebas posibles, aunque las mismas no son completas:

*Main> (gradoDeDiversion.videoJuegos 5) mariano
202

*Main> (gradoDeDiversion.jardineria "bonsai") mariano
220

*Main> esCentennial mariano
False

*Main> esCentennial (Persona "Nil" (7, 5, 2013) 0 [])
True

*Main> (nombre.golf) mariano
"Sir Mariano"

*Main> (nombre.golf.golf) mariano
"Sir Mariano"
-}

