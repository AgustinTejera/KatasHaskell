module Lib where
import Text.Show.Functions
type Promedio = Float  
type Recurso = String
type Estrategia = Pais->Pais
someFunc :: IO ()
someFunc = putStrLn "someFunc"
        
data Pais= Pais{
ingresoPerCapita::Promedio,
poblacionPublica::Int,
poblacionPrivada::Int,
recursos::[Recurso],
deuda::Float 
}deriving (Eq,Show)
        
{-
Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s,
la población activa del sector público es de 400.000, la población activa del sector privado 
es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.
-}
        
namibia = Pais{
ingresoPerCapita=4140,
poblacionPublica=400000,
poblacionPrivada=650000,
recursos = ["Mineria","Ecoturismo"],
deuda = 50  --PORQUE DICE "50" SOLAMENTE NO IMPORTA QUE UNIDAD SEA!!!
} 
    
argentina = Pais{
ingresoPerCapita=2020,
poblacionPublica=40,
poblacionPrivada=650,
recursos = ["Mineria","Ecoturismo","Petroleo"],
deuda = 50  --PORQUE DICE "50" SOLAMENTE NO IMPORTA QUE UNIDAD SEA!!!
} 
    
brasil = Pais{
ingresoPerCapita=2023,
poblacionPublica=400000,
poblacionPrivada=650000,
recursos = ["Mineria","Ecoturismo","Petroleo"],
deuda = 50  --PORQUE DICE "50" SOLAMENTE NO IMPORTA QUE UNIDAD SEA!!!
} 
    
---------------------------------------------RECETASSSS!!!!!!!!!!!!!!!!!!!!!!!!
--1_a)prestarle n millones de dólares al país, esto provoca que el país se endeude en un 150% de lo que el FMI le presta
-- (por los intereses)    
----------------------------1_A)
prestarleCantidad::Float->Estrategia
prestarleCantidad cantidad pais = pais{deuda=deuda pais + intereses cantidad}
    
intereses = (*1.5)

----------------------------1_B)
reducirPuestos::Int->Estrategia
reducirPuestos cantidad pais =  pais{
    poblacionPublica = poblacionPublica pais - cantidad,
    ingresoPerCapita = ingresoPerCapita pais - (ingresoPerCapita pais * disminuir cantidad)
}
    -- ingresoPerCapita = (ingresoPerCapita pais - disminuir cantidad) --ESTO ES EL 20% del ingresoPerCapita
    
disminuir::Int->Float
disminuir cantidad
 |cantidad>100 = 0.2
 |otherwise = 0.15
 
 
----------------------------1_C)
--darle a una empresa afín al FMI la explotación de alguno de los recursos naturales, esto disminuye 2 
--millones de dólares la deuda que el país mantiene con el FMI pero también deja momentáneamente sin
-- recurso natural a dicho país. No considerar qué pasa si el país no tiene dicho recurso.

explotar::Recurso->Estrategia
explotar recurso pais = pais{
deuda=deuda pais-2000000,    --ELLOS PONEN 20, PARA MI ES 2!!!!!!!!!!!!
recursos = quitarRecurso recurso $ recursos pais} 
        
quitarRecurso::Recurso->[Recurso]->[Recurso]
quitarRecurso recurso recursos = filter (/=recurso) recursos

    
----------------------------1_D)
    
blindaje::Estrategia
blindaje pais = (prestarleCantidad (productoBrutoInterno pais * 0.5).reducirPuestos 500) pais
blindaje1 pais = ((reducirPuestos 500).(prestarleCantidad (productoBrutoInterno pais * 0.5))) pais
--  LE PONGO 0.5 QUE ES LO MISMO QUE DIVIDIR POR 2
        
--PORQUE ingresoPerCapita ES UN FLOAT
productoBrutoInterno::Pais->Float
productoBrutoInterno pais = ingresoPerCapita pais * fromIntegral(poblacionActiva pais) 
    
poblacionActiva::Pais->Int
poblacionActiva pais = poblacionPublica pais + poblacionPrivada pais
    
----------------------------------------PUNTO 3!!!!!!!!!!!!---------------------
    
-----------------------3_A)
type Receta = [Estrategia]
receta::Receta
receta = [
    prestarleCantidad 200,
    explotar "Mineria"
 ]
    
receta2 = [
    prestarleCantidad 150,
    explotar "caca"
 ]
    
-----------------------3_B) 
aplicarReceta::Receta->Estrategia
aplicarReceta receta pais = foldr ($) namibia receta
    
    
-----------------------PUNTO 4
-- 4)Resolver todo el punto con orden superior, composición y aplicación parcial, no puede utilizar funciones auxiliares.
-- 4.a)Dada una lista de países conocer cuáles son los que pueden zafar, aquellos que tienen "Petróleo" entre 
--     sus riquezas naturales.
    
-------------------------------FORMA QUE PIDE EJERCICIO!!!!
    
puedenSafar::[Pais]->[Pais]
puedenSafar  = filter (elem "petroleo".recursos)
    
    
--------------------------------FORMA NORMAL!!!!
puedenSafar1 = filter safa 
    
safa  = elem "petroleo".recursos
    
--------------------------------FORMA CON LAMBDA!!!!
    
puedenSafar2 paises = filter (\pais -> elem "Petróleo" (recursos pais)) paises
    
puedenSafar23 = filter (\pais -> (elem "Petróleo".recursos) pais)
-- 4)Resolver todo el punto con orden superior, composición y aplicación parcial, no puede utilizar funciones auxiliares.
-- 4.b)Dada una lista de países, saber el total de deuda que el FMI tiene a su favor.
--------------------------------MI MANERA!!!!

deudaTotal::[Pais]->Float
deudaTotal = sum.map deuda
    
--------------------------------MANERA PRO!!!!
deudaTotal1::[Pais]->Float
deudaTotal1 paises = foldr ((+).deuda) 0 paises
    
totalDeuda :: [Pais] -> Float
totalDeuda = foldr ((+) . deuda) 0 
    
    
-- 4.c)Indicar en donde apareció cada uno de los conceptos (solo una vez) y justificar qué ventaja tuvo para resolver 
-- el requerimiento.
{-                                                            
Aparece el concepto de composicion, en "sum.map deuda", por ejemplo, ya que la imagen de la funcion map coincide con el dominio
de la funcion sum, por lo que se pueden componer sin ningun tipo de problemas. También aparece el concepto de aplicacion parcial
en, por ejemplo, "map deuda" , ya que esta funcion espera 2 parametros, y solo le estamos pasando el parametro deuda, 
esperando recibir al segundo parametro paises. Sin el concepto de aplicacion parcial no hubiese sido posible resolverlo 
de esta manera. Ademas aparece el concepto de orden superior porque estamos usando funciones como "filter" o "map",
que pueden recibir como parametro una funcion, como tambien devolver otra funcion, lo que se denomina orden superior.
La ventaja para resolver este requerimiento es que gracias al orden superior nos evitamos repetir codigo constantemente,
delegando la responsabilidad a una unica funcion
 la ventaja es que permiten construir funciones más generales, recibiendo funciones que abstraen porciones de código. 
-}
    
    
{-
------------------------------------------PUNTO 5!!!!------------------------------------- 
(2 puntos) Debe resolver este punto con recursividad: dado un país y una lista de recetas, saber
 si la lista de recetas 
está ordenada de “peor” a “mejor”, en base al siguiente criterio: 
si aplicamos una a una cada receta, el PBI del país va
de menor a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per
 cápita por la población 
activa (privada y pública). 
-}

------------------------------------------REPASAR!!!
estaOrdenado _ [receta] = True
estaOrdenado pais (receta1:receta2:recetas)
  = criterioPBI receta1 pais <=  criterioPBI receta2 pais && estaOrdenado pais (receta2:recetas)
       where criterioPBI receta = productoBrutoInterno.aplicarReceta receta

    
------------------------------------------PUNTO 6!!!
{-    ---------------------------6.a)
Si un país tiene infinitos recursos naturales, modelado con esta función
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
¿qué sucede evaluamos la función 4a con ese país? 
No se puede resolver porque nunca va a terminar de evaluar la lista infinita de recursos, se 
produce un loop infinito, ya que va a buscar "Petróleo" entre todos los recursos, y 
nunca termina de evaluar.
-}
    
recursosNaturalesInfinitos :: [String]
recursosNaturalesInfinitos = "Energia" : recursosNaturalesInfinitos
    
pruebaInfinita1 = puedenSafar [Pais 10 5000 4000 recursosNaturalesInfinitos 20]
    
{-
¿y con la 4b?
Justifique ambos puntos relacionándolos con algún concepto.
En este caso, por mas que sea una lista infinita la de recursos, gracias a lazy evaluation(evaluacion diferida)
se puede resolver esta funcion y evaluar solamente lo que nos importa, la deuda. 
Dejando de lado la lista infinita de recursos 
-}
    
pruebaInfinita2 = deudaTotal [Pais 10 5000 4000 recursosNaturalesInfinitos 20]


f2 x _ [] = x
f2 x y (z:zs)
    | y z > 0   = z + f2 x y zs
    | otherwise = f2 x y zs

-- f::(Ord a,Num a,Num b)=>a->(a->b)->[a]->a

