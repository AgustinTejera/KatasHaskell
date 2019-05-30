{-
y el criterio para estudiar ante un parcial:
algunos son estudiosos: estudian siempre, 
otros son hijos del rigor: estudian si el parcial tiene más de n preguntas, 
y también están los cabuleros, que estudian si la materia tiene una cantidad impar de letras.
-}
--type Parcial = (Materia,cantidadDePreguntas)
--type CantidadDePreguntas = Int

import Text.Show.Functions 
type Dia= Int
type Mes= Int
type Anio= Int
type Materia= String
type CriterioDeEstudio = Parcial -> Bool

data Alumno = Alumno{
nombre::String,
fechaDeNac::(Dia,Mes,Anio),
legajo::Int,
materias::[Materia],
criterioDeEstudio::CriterioDeEstudio
} deriving (Show)

data Parcial = Parcial{
materia::Materia,
cantidadDePreguntas::Int
} deriving (Show)


estudioso::CriterioDeEstudio
estudioso _ = True

hijoDelRigor::Int->CriterioDeEstudio
hijoDelRigor n parcial= cantidadDePreguntas parcial > n

cabuleros::CriterioDeEstudio
cabuleros parcial = (odd.length.materia) parcial


belen = Alumno{
nombre="Belen",
fechaDeNac=(30,01,1992),
legajo=1616470,
materias=["algebra"],
criterioDeEstudio=cabuleros
}

belen = Alumno{
nombre="Agustin",
fechaDeNac=(16,11,1996),
legajo=1616420,
materias=["PDP"],
criterioDeEstudio=hijoDelRigor 5  --IMPORTANTE PASARLE EL PARAMETRO HARDCODEADO
}  

parcialito = Parcial{
materia="Algebra",
cantidadDePreguntas=4
} 

cambiarCriterioEstudio::Alumno->CriterioDeEstudio->Alumno
cambiarCriterioEstudio alumno criterioNuevo = alumno{criterioDeEstudio= criterioNuevo}

estudia::Alumno->Parcial->Bool
estudia alumno parcial = (criterioDeEstudio alumno) parcial 
