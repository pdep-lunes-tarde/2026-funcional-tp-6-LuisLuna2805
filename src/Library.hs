module Library where
import PdePreludat
import GHC.InfoProv (InfoProv)
import GHC.Num (integerAnd)

data Ingrediente =
    Carne | Pan | Panceta | Cheddar | Pollo | Curry | QuesoDeAlmendras | Papas | PatyVegano | Tofu | PanIntegral
    deriving (Eq, Show)

precioIngrediente Carne = 20
precioIngrediente Pollo =  10
precioIngrediente PatyVegano = 10
precioIngrediente Pan = 2
precioIngrediente PanIntegral = 3
precioIngrediente Panceta = 10
precioIngrediente Tofu = 10
precioIngrediente Cheddar = 10
precioIngrediente QuesoDeAlmendras = 15
precioIngrediente Curry = 5
precioIngrediente Papas = 10

data Hamburguesa = Hamburguesa {
    precioBase :: Number,
    ingredientes :: [Ingrediente]
} deriving (Eq, Show)


--Parte 1 
cuartoDeLibra :: Hamburguesa 
cuartoDeLibra = Hamburguesa {
    precioBase = 20,
    ingredientes = [Pan, Carne, Cheddar, Pan]
}

pdepBurger :: Hamburguesa
pdepBurger =  agrandarMultiple 2 . agregarIngredientes [Panceta, Cheddar] $ cuartoDeLibra

--Caclula el precio de los ingredientes de una hamburguesa
precioIngredientes :: [Ingrediente] -> Number
precioIngredientes [] = 0
precioIngredientes (x:xs) = precioIngrediente x + precioIngredientes xs 

--Calcula precio final de una hamburguesa
calculoNormal :: Hamburguesa -> Number
calculoNormal hamburguesa = precioBase hamburguesa + precioIngredientes (ingredientes hamburguesa)

descuento :: Number -> Hamburguesa -> Number
descuento porcentaje hamburguesa = (calculoNormal hamburguesa) * (100 - porcentaje)/100

--Calcula precio final de una hamburguesa
precioFinal :: Hamburguesa -> Number
precioFinal hamburguesa
    |hamburguesa == pdepBurger = descuento 20 pdepBurger
    |elem Papas (ingredientes hamburguesa) = descuento 30 hamburguesa --si en algun momento otro pide papas tambien le aplicaria (mal pero no se me ocurre otra cosa)
    |otherwise = calculoNormal hamburguesa

--Definir que medallon tiene primero
detectarMdeallon :: [Ingrediente] -> Ingrediente
detectarMdeallon [] = undefined
detectarMdeallon (x:xs)   
    |   x == Carne  = Carne
    |   x == Pollo  = Pollo
    |   x == PatyVegano = PatyVegano
    |   otherwise = detectarMdeallon xs  

--Agregarle un medallon del que ya tenga (si hay mas de uno el primero que vea)
agrandar :: Hamburguesa -> Hamburguesa
agrandar hamburguesa = agregarIngrediente (detectarMdeallon (ingredientes hamburguesa)) hamburguesa

agrandarMultiple :: Number -> Hamburguesa -> Hamburguesa
agrandarMultiple 0 hamburguesa = hamburguesa
agrandarMultiple cantidad hamburguesa = agrandarMultiple (cantidad-1) (agrandar hamburguesa)

--Agregar un ingrediente cualquiera a mi hamburguesa
agregarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa
agregarIngrediente nuevoIngrediente hamburguesa = 
    hamburguesa {ingredientes = ingredientes hamburguesa ++ [nuevoIngrediente]}

agregarIngredientes :: [Ingrediente] -> Hamburguesa -> Hamburguesa
agregarIngredientes [] hamburguesa = hamburguesa
agregarIngredientes (x:xs) hamburguesa = agregarIngredientes xs (agregarIngrediente x hamburguesa)

agregarIngredientesPorCantidad :: [Ingrediente] -> Number -> Hamburguesa -> Hamburguesa
agregarIngredientesPorCantidad [] _ hamburguesa = hamburguesa
agregarIngredientesPorCantidad [x] 0 hamburguesa = hamburguesa
agregarIngredientesPorCantidad (x:xs) numero hamburguesa = agregarIngredientesPorCantidad (x:xs) (numero - 1) (agregarIngrediente x hamburguesa)

--Parte 2
dobleCuarto :: Hamburguesa
dobleCuarto =  agrandar . agregarIngrediente Cheddar $ cuartoDeLibra

bigPdep :: Hamburguesa
bigPdep =  agregarIngrediente Curry dobleCuarto

--Le agrega papas y descuento del 30 a una hamburguesa
delDia :: Hamburguesa -> Hamburguesa
delDia hamburguesa = agregarIngrediente Papas hamburguesa --Y CAMBIARLE EL NOMBRE

--Parte 3
-- 

reemplazo :: Ingrediente -> Ingrediente 
reemplazo ingredienteViejo 
    |   ingredienteViejo == Cheddar = QuesoDeAlmendras
    |   ingredienteViejo == Pollo = PatyVegano
    |   ingredienteViejo == Carne = PatyVegano
    |   ingredienteViejo == Panceta = Tofu
    |   ingredienteViejo == Pan = PanIntegral
    |   otherwise = undefined

cuantosSaque :: Hamburguesa -> Hamburguesa -> Number
cuantosSaque original filtrada = length (ingredientes original) - length (ingredientes filtrada) 

sacarIngrediente:: Ingrediente -> Hamburguesa -> Hamburguesa
sacarIngrediente viejoIngrediente hamburguesa = 
    hamburguesa  {ingredientes = filter (/= viejoIngrediente) (ingredientes hamburguesa)}

esNecesarioAgregar :: Ingrediente -> Hamburguesa -> [Ingrediente]
esNecesarioAgregar ingrediente hamburguesa
    |elem ingrediente (ingredientes hamburguesa) = [reemplazo ingrediente]  
    |otherwise = []

cambiarIngrediente :: Ingrediente -> Hamburguesa -> Hamburguesa 
cambiarIngrediente ingrediente hamburguesa = 
    agregarIngredientesPorCantidad (esNecesarioAgregar ingrediente hamburguesa) (cuantosSaque hamburguesa (sacarIngrediente ingrediente hamburguesa)) (sacarIngrediente ingrediente hamburguesa) 

hacerVeggie :: Hamburguesa -> Hamburguesa
hacerVeggie = cambiarIngrediente Cheddar . cambiarIngrediente Panceta . cambiarIngrediente Carne . cambiarIngrediente Pollo

-- cambiar pon pan integral -> Precio 3
cambiarPanDePaty :: Hamburguesa -> Hamburguesa
cambiarPanDePaty = cambiarIngrediente Pan
-- falta considerar la existencia de mas de uno (con cada elemento lo mismo) (contar la cantidad que saca y agregar esa misma cantidad)

-- doble cuarto pero veggie con pan integral
dobleCuartoVegano :: Hamburguesa
dobleCuartoVegano = cambiarPanDePaty (hacerVeggie dobleCuarto) 

