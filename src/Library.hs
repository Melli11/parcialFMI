{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Library where
import PdePreludat
import GHC.Generics (Par1(unPar1))

doble :: Number -> Number
doble numero = numero + numero
data RecursoNatural = Mineria | Petroleo | IndustriaPesada | Ganaderia | Ecoturismo deriving (Show,Eq)

data Pais = Pais {
    ingresoPerCapita :: Number,
    sectorPublico :: Number,
    sectorPrivado :: Number,
    recursos :: [RecursoNatural],
    deuda :: Number
} deriving (Show,Eq)


-- RECETAS

-- prestarle n millones de dólares al país, esto provoca que el país se endeude en
-- un 150% de lo que el FMI le presta (por los intereses)

prestarleNMillones:: Number -> Pais -> Pais
prestarleNMillones cantidad  =  cambiarLaDeuda (cantidad*1.5)

-- reducir x cantidad de puestos de trabajo del sector público, lo que provoca que
-- se reduzca la cantidad de activos en el sector público y además que el ingreso
-- per cápita disminuya en 20% si los puestos de trabajo son más de 100 ó 15% en
-- -- -- caso contrario
reducirSectorPublicoEn :: Number -> Pais -> Pais
reducirSectorPublicoEn cantidad unPais
    | sectorPublico unPais > 100 = cambiarSectorPublico (negate cantidad) $ cambiarIngresoPerCapital (negate (0.2 * ingresoPerCapita unPais)) unPais
    | otherwise =  cambiarSectorPublico (negate cantidad) $ cambiarIngresoPerCapital(negate (0.15 * ingresoPerCapita unPais)) unPais 

darEmpresaAlFmi :: RecursoNatural -> Receta
darEmpresaAlFmi recursoNatural  = explotarRecurso recursoNatural .  cambiarLaDeuda (negate 20000000)

-- establecer un “blindaje”, lo que provoca prestarle a dicho país la mitad de su
-- Producto Bruto Interno (que se calcula como el ingreso per cápita multiplicado
-- por su población activa, sumando puestos públicos y privados de trabajo) y
-- reducir 500 puestos de trabajo del sector público. Evitar la repetición de código.

establecerBlindaje :: Pais -> Pais
establecerBlindaje unPais = prestarleNMillones (productoBrutoInterno unPais) $ cambiarSectorPublico (negate 500) unPais

productoBrutoInterno :: Pais -> Number
productoBrutoInterno unPais = ingresoPerCapita unPais + sectorPrivado unPais + sectorPrivado unPais

-- Dar un ejemplo de cómo generar al país Namibia, cuyo ingreso per cápita
-- es de 4140 u$s, la población activa del sector público es de 400.000, la
-- población activa del sector privado es de 650.000, su riqueza es la minería
-- y el ecoturismo y le debe 50 (millones de u$s) al FMI.

namibia :: Pais
namibia = Pais 4140 400 650000 [Mineria,Ecoturismo] 50000000

bolivia :: Pais
bolivia = Pais 4140 400 650000 [Petroleo,Ecoturismo] 40000000

venezuela :: Pais
venezuela = Pais 4140 400 650000 [Petroleo,Ecoturismo] 30000000

brasil :: Pais
brasil = Pais 4140 400 650000 [Ganaderia,Ecoturismo] 20000000


-- Funciones Auxiliares

type Receta = Pais -> Pais
explotarRecurso :: RecursoNatural -> Receta
explotarRecurso recursoNatural unPais = unPais { recursos = filter (recursoNatural/=) $recursos unPais  }
cambiarLaDeuda :: Number -> Pais -> Pais
cambiarLaDeuda  cantidad unPais =  unPais { deuda = deuda unPais + cantidad }

cambiarSectorPublico :: Number -> Pais -> Pais
cambiarSectorPublico cantidad unPais = unPais {sectorPublico = sectorPublico unPais + cantidad}

cambiarIngresoPerCapital :: Number -> Pais -> Pais
cambiarIngresoPerCapital cantidad unPais = unPais {ingresoPerCapita = ingresoPerCapita unPais + cantidad}



-- Paises Auxiliares
argentina :: Pais
argentina = Pais 5 10 10 [Mineria, Petroleo] 10000000

listaDePaises = [brasil,venezuela,bolivia,argentina,namibia]
-- a.Dada una lista de países conocer cuáles son los que pueden zafar,
-- aquellos que tienen "Petróleo" entre sus riquezas naturales.

puedenZafar :: [Pais] -> [Pais]
puedenZafar paises = filter (\pais  -> elem Petroleo $ recursos pais ) paises


-- b. Dada una lista de países, saber el total de deuda que el FMI tiene a su
-- favor.

totalDeDeuda  = sum.map (deuda) 
-- c. Indicar en dónde apareció cada uno de los conceptos (solo una vez) y
-- justificar qué ventaja tuvo para resolver el requerimiento.

-- Debe resolver este punto con recursividad: dado un país y una lista de
-- recetas, saber si la lista de recetas está ordenada de “peor” a “mejor”, en base al
-- siguiente criterio: si aplicamos una a una cada receta, el PBI del país va de menor
-- a mayor. Recordamos que el Producto Bruto Interno surge de multiplicar el
-- ingreso per cápita por la población activa (privada y pública).
