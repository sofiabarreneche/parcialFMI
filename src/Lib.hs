module Lib where
import Text.Show.Functions

laVerdad = True

-- Punto 1

type Poblacion = Int
type Recursos = String
type PaiSes =  Pais -> Pais 

data Pais = Pais {
ingresopercapita :: Float,
pobsectorpublico :: Poblacion,
pobsectorprivado :: Poblacion,
recursosnaturales :: [Recursos],
deuda :: Float
} deriving (Show, Eq)

-- No sabemos como hacer millones XD asi que son miles LOL
namibia :: Pais 
namibia = Pais 4140 400000 650000 ["Mineria", "Ecoturismo"] 5000

-- Punto 2

prestarDinero :: Float -> PaiSes
prestarDinero dolares pais = pais {deuda = deuda pais + (sacarPorcentaje dolares 150)}

sacarPorcentaje :: Float -> Float -> Float
sacarPorcentaje dolares porcentaje = (dolares * porcentaje) / 100 

--
disminuirPerCapita :: PaiSes
disminuirPerCapita pais | ((>100).pobsectorpublico) pais  = pais  {ingresopercapita = ingresopercapita pais - (sacarPorcentaje (ingresopercapita pais) 20) }
                        | otherwise = pais  {ingresopercapita = ingresopercapita pais - (sacarPorcentaje (ingresopercapita pais) 15) }

reducirActivos ::Int -> PaiSes 
reducirActivos  cantidadXParaReducir pais = pais {pobsectorpublico = pobsectorpublico pais - cantidadXParaReducir}

reducirCantidadDePuestosDeTrabajo :: Int -> PaiSes
reducirCantidadDePuestosDeTrabajo cantidadParaReducir = disminuirPerCapita . (reducirActivos cantidadParaReducir)

--
explotacionDeRecurso :: Recursos -> PaiSes
explotacionDeRecurso recurso = (sacarRecurso recurso).(restarDolaresALaDeuda 2000)

sacarRecurso :: Recursos -> PaiSes
sacarRecurso recurso pais = pais {recursosnaturales = filter (/= recurso ) (recursosnaturales pais)} 

restarDolaresALaDeuda :: Float -> PaiSes 
restarDolaresALaDeuda dinero pais = pais {deuda = deuda pais - dinero}

establecerUnBlindaje :: PaiSes
establecerUnBlindaje pais =( prestarDinero (productoBruto pais * 0.5 ) .reducirActivos 500)  pais

productoBruto :: Pais -> Float
productoBruto  pais = ingresopercapita pais * fromIntegral (sumaDePobActiva pais)

sumaDePobActiva :: Pais -> Int
sumaDePobActiva pais = pobsectorpublico pais + pobsectorprivado pais

--Punto 3

recetaModelo ::  [PaiSes]
recetaModelo = [prestarDinero 20000, explotacionDeRecurso "Minería"]

aplicarRecetaModelo :: [PaiSes] -> PaiSes
aplicarRecetaModelo recetas pais = foldl aplicarUnaReceta pais recetas

aplicarUnaReceta :: Pais -> PaiSes -> Pais
aplicarUnaReceta  pais receta = receta pais 

--Punto 4
--Dada una lista de países, saber el total de deuda que el FMI tiene a su favor. MATEO

paisesQueZafan :: [Pais] -> [Pais]
paisesQueZafan  = filter $ elem "Petroleo" . recursosnaturales 

deudaPaises :: [Pais] -> Float
deudaPaises  = foldr ((+) . deuda ) 0

--Debe resolver este punto con recursividad: dado un país y una lista de recetas,
--saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al siguiente
--criterio: si aplicamos una a una cada receta, el PBI del país va de menor a mayor.
--Recordamos que el Producto Bruto Interno surge de multiplicar el ingreso per cápita por la población activa (privada y pública). 

--recetasOrdenadas ::