module Lib where
import Text.Show.Functions
type Promedio = Float  
type Recurso = String
someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Pais= Pais{
ingresoPerCapita::Promedio,
poblacionPublica::Int,
poblacionPrivada::Int,
recursos::[Recurso],
deuda::Float 
}deriving (Eq, Show)

{-
Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita es de 4140 u$s,
 la población activa del sector público es de 400.000, la población activa del sector privado 
 es de 650.000, su riqueza es la minería y el ecoturismo y le debe 50 (millones de u$s) al FMI.
-}

namibia = Pais{
ingresoPerCapita=4140,
poblacionPublica=400000,
poblacionPrivada=650000,
recursos = ["mineria","ecoturismo"],
deuda = 50
} 


prestarleCantidad dolares pais = pais{deuda=deuda pais + (dolares*1.5)}
--
-- 2.b 
-- Reducir x puestos de trabajo del sector publico


{---------------------------- DUDA
reducirPuestos :: Int -> Estrategia
reducirPuestos cantidadPuestos pais = pais {
    activosPublico = activosPublico pais - cantidadPuestos,
    ingresoPerCapita = ingresoPerCapita pais * (1 - reduccionIngreso cantidadPuestos)
}
-}

reducirPuestos cantidad pais =  pais{
    poblacionPublica = poblacionPublica pais - cantidad,
    ingresoPerCapita = ingresoPerCapita pais - disminuir cantidad
    }  

disminuir cantidad
 |cantidad>100 = 0.2
 |otherwise = 0.15

--MAL!!!
darEmpresaAfin pais = pais{deuda=deuda pais-2000000,recursos = []} 

--  LE PONGO 0.5 QUE ES LO MISMO QUE DIVIDIR POR 2
--blindaje pais=  
--    ((reducirPuestos 500).(prestarleCantidad (calcularProductoBrutoInterno pais*0.5))) pais   
{-
establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su Producto Bruto
 Interno (que se calcula como el ingreso per cápita multiplicado por su población activa, 
 sumando puestos públicos y privados de trabajo) y reducir 500 puestos de trabajo del sector
  público. Evitar la repetición de código.
-}
calcularProductoBrutoInterno::Pais->Float
calcularProductoBrutoInterno pais = (ingresoPerCapita pais * fromIntegral (poblacionActiva pais)) * 0.5 

poblacionActiva::Pais->Int
poblacionActiva pais = poblacionPublica pais + poblacionPrivada pais
