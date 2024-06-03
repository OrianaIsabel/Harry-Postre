module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

-- Punto 1.A
data Postre = UnPostre {
    sabores :: [Sabor],
    peso :: Number,
    temperatura :: Number
} deriving (Show,Eq)

type Sabor = String

bizcochoBorracho :: Postre
bizcochoBorracho = UnPostre ["Fruta","Crema"] 100 25

alfajor :: Postre
alfajor = UnPostre ["Galleta","Dulce de Leche","Chocolate"] 30 10

tartaDeMelaza :: Postre
tartaDeMelaza = UnPostre ["Crema","Melaza"] 50 0

suspiro :: Postre
suspiro = UnPostre [] 10 5

macaron :: Postre
macaron = UnPostre ["Azucar"] 10 10

-- Punto 1.B
incendio :: Postre -> Postre
incendio = (perderPeso 5).(calentar 1)

calentar :: Number -> Postre -> Postre
calentar incremento postre = postre { temperatura = (temperatura postre) + incremento }

perderPeso :: Number -> Postre -> Postre
perderPeso porcentaje postre = postre { peso = (peso postre) * porcentaje / 100 }

immobulus :: Postre -> Postre
immobulus = congelar

congelar :: Postre -> Postre
congelar postre = postre { temperatura = 0 }

wingardiumLeviosa :: Postre -> Postre
wingardiumLeviosa = (perderPeso 10).(agregarSabores ["Concentrado"])

agregarSabores :: [Sabor] -> Postre -> Postre
agregarSabores nuevosSabores postre = postre { sabores = (++) (sabores postre) nuevosSabores }

diffindo :: Number -> Postre -> Postre
diffindo = perderPeso

riddikulus :: Sabor -> Postre -> Postre
riddikulus sabor postre = (invertirSabores.(agregarSabores [sabor])) postre

invertirSabores :: Postre -> Postre
invertirSabores postre = postre { sabores = invertirLista (sabores postre)}

invertirLista :: [String] -> [String]
invertirLista [] = []
invertirLista (x:xs) = (invertirLista xs) ++ [x]

avadaKedavra :: Postre -> Postre
avadaKedavra = perderSabores.congelar 

perderSabores :: Postre -> Postre
perderSabores postre = postre { sabores = [] }

-- Punto 1.C
estanListosYHechizo :: (Postre -> Postre) -> [Postre] -> Bool
estanListosYHechizo hechizo postres = all estaListo (map hechizo postres)

estaListo :: Postre -> Bool
estaListo postre = pesaMasQueCero postre && tieneAlgunSabor postre && noEstaCongelado postre

pesaMasQueCero :: Postre -> Bool
pesaMasQueCero postre
    | (peso postre) > 0 = True
    | otherwise = False

todosPesanMasQueCero :: [Postre] -> Bool
todosPesanMasQueCero = all pesaMasQueCero

tieneAlgunSabor :: Postre -> Bool
tieneAlgunSabor postre
    | (sabores postre) == [] = False
    | otherwise = True

todosTienenAlgunSabor :: [Postre] -> Bool
todosTienenAlgunSabor = all tieneAlgunSabor

noEstaCongelado :: Postre -> Bool
noEstaCongelado postre
    | (temperatura postre) <= 0 = False
    | otherwise = True

ningunoEstaCongelado :: [Postre] -> Bool
ningunoEstaCongelado = all noEstaCongelado

-- Punto 1.D
promedioDeListos :: [Postre] -> Number
promedioDeListos = promedio.listaDePesos.listaDeListos

listaDeListos :: [Postre] -> [Postre]
listaDeListos = filter estaListo

promedio :: [Number] -> Number
promedio lista = sum lista / length lista

listaDePesos :: [Postre] -> [Number]
listaDePesos = map peso

-- Punto 2
data Mago = UnMago {
    hechizos :: [Hechizo],
    cantidadHorrorcruzes :: Number
} deriving Show

pam :: Mago
pam = UnMago [(riddikulus "papaya"),immobulus,incendio] 6

type Hechizo = Postre -> Postre

-- Punto 2.A
practicarHechizo :: Hechizo -> Mago -> Postre -> Mago
practicarHechizo hechizo mago postre
    | (hechizo postre) == (avadaKedavra postre) = (agregarHorrocruz.(agregarHechizo hechizo)) mago
    | otherwise = agregarHechizo hechizo mago

agregarHechizo :: Hechizo -> Mago -> Mago
agregarHechizo hechizo mago = mago { hechizos = (hechizo : hechizos mago)}

agregarHorrocruz :: Mago -> Mago
agregarHorrocruz mago = mago { cantidadHorrorcruzes = (cantidadHorrorcruzes mago) + 1 }

-- Punto 2.B
mejorHechizo :: Postre -> Mago -> Hechizo
mejorHechizo postre mago = elMejorDeTodos (hechizos mago) postre

elMejorDeTodos :: [Hechizo] -> Postre -> Hechizo
elMejorDeTodos listaHechizos postre = foldl1 (cualEsMejor postre) listaHechizos

cualEsMejor :: Postre -> Hechizo -> Hechizo -> Hechizo
cualEsMejor postre hechizo1 hechizo2
    | ((length.sabores.hechizo1) postre) > ((length.sabores.hechizo2) postre) = hechizo1
    | otherwise = hechizo2

-- Punto 3
postresInfinitos :: Postre -> [Postre]
postresInfinitos postre = ( postre : postresInfinitos postre )

alfajorInfinito :: [Postre]
alfajorInfinito = postresInfinitos alfajor

hechizosInfinitos :: Hechizo -> [Hechizo]
hechizosInfinitos hechizo = ( hechizo : hechizosInfinitos hechizo )

magoInfinito :: Mago
magoInfinito = UnMago ( hechizosInfinitos immobulus ) 10