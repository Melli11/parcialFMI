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
-- reducirSectorPublicoEn :: Number -> Pais -> Pais
-- reducirSectorPublicoEn cantidad unPais
--     | sectorPublico unPais > 100 = cambiarSectorPublico ( negate cantidad ) .  cambiarIngresoPerCapital (negate 0.2 * (ingresoPerCapita unPais) )
--     | otherwise =  cambiarSectorPublico ( negate cantidad) .  cambiarIngresoPerCapital(negate 0.15 * ingresoPerCapita unPais) 

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
argentina = Pais 5 10 10 [Mineria, Petroleo] 1500000
