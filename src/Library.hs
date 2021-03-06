module Library where
import PdePreludat


-- Punto 1
data Raton = UnRaton {
    nombre :: String
    , edad :: Number
    , peso :: Number
    , enfermedades :: [String]
} deriving (Show, Eq)

cerebro = UnRaton {
    nombre = "Cerebro"
    , edad = 9
    , peso = 0.2 
    , enfermedades = ["brucelosis", "sarampion", "tuberculosis"]
}

bicenterrata = UnRaton {
    nombre = "Bicenterrata"
    , edad = 256
    , peso = 0.2 
    , enfermedades = []
}

huesudo = UnRaton {
    nombre = "Huesudo"
    , edad = 4
    , peso = 10 
    , enfermedades = ["alta obesidad", "sinusitis"]
}

orejita = UnRaton {
    nombre = "Orejita"
,   edad=24
,   peso=0.325
,   enfermedades =["Estetococos", "Condetodo", "Esterlaisis", "Pulmonia","Covid","Malaria", "Anemia","Uretritis","Ceguera"]
}

-- Punto 2
type Hierba = Raton -> Raton 
----------------------------------------------------------------------------------------------------------------------------------------------
hierbaBuena :: Hierba
hierbaBuena = raizCuadradaDeLaEdad

raizCuadradaDeLaEdad :: Raton -> Raton
raizCuadradaDeLaEdad raton = raton {edad = sqrt (edad raton)}
----------------------------------------------------------------------------------------------------------------------------------------------
type Terminacion = String
type Enfermedad = String

hierbaVerde :: Terminacion -> Hierba
hierbaVerde = eliminarEnfermedadesIgualFinal

eliminarEnfermedadesIgualFinal :: Terminacion -> Raton -> Raton
eliminarEnfermedadesIgualFinal terminacion raton = raton {enfermedades = enfermedadesDistintoFinal terminacion (enfermedades raton)} 

enfermedadesDistintoFinal :: Terminacion -> [Enfermedad] -> [Enfermedad]
enfermedadesDistintoFinal terminacion = filter (not.tieneIgualFinal terminacion) 

tieneIgualFinal :: Terminacion -> Enfermedad -> Bool 
tieneIgualFinal terminacion enfermedad = terminacion == reverse (take (length terminacion) (reverse enfermedad))
----------------------------------------------------------------------------------------------------------------------------------------------
alcachofa :: Hierba
alcachofa raton 
    | peso raton > 2 = modificarPeso (-(porcentajeDelPeso 10 raton)) raton
    | otherwise = modificarPeso (-(porcentajeDelPeso 5 raton)) raton

modificarPeso :: Number -> Raton -> Raton 
modificarPeso numero raton = raton {peso = (max 0 (peso raton + numero))}

porcentajeDelPeso :: Number -> Raton -> Number
porcentajeDelPeso porCiento raton = (porCiento/100) * (peso raton)
----------------------------------------------------------------------------------------------------------------------------------------------
hierbaZort :: Hierba
hierbaZort = quedarConCeroAnios . perderTodasLasEnfermedades . (cambiarNombreRaton "pinky")

cambiarNombreRaton :: String -> Raton -> Raton
cambiarNombreRaton nuevoNombre raton = raton {nombre = nuevoNombre}

perderTodasLasEnfermedades :: Raton -> Raton
perderTodasLasEnfermedades raton = raton {enfermedades = []}

quedarConCeroAnios :: Raton -> Raton
quedarConCeroAnios raton = raton {edad = 0}
----------------------------------------------------------------------------------------------------------------------------------------------
hierbaDelDiablo :: Hierba
hierbaDelDiablo = eliminarEnfermedadesConMenosDeDiezLetras . (modificarPeso (-0.1))
-- Para que el peso no sea menor a 0 lo inclu?? en modificarPeso

eliminarEnfermedadesConMenosDeDiezLetras :: Raton -> Raton
eliminarEnfermedadesConMenosDeDiezLetras raton = raton {enfermedades = enfermedadesConMasDeDiezLetras (enfermedades raton)}

enfermedadesConMasDeDiezLetras :: [Enfermedad] -> [Enfermedad]
enfermedadesConMasDeDiezLetras = filter masDeDiezLetras 

masDeDiezLetras :: Enfermedad -> Bool
masDeDiezLetras = (>10) . length 

-- PUNTO 3
type Medicamento = [Hierba]

aplicarHierba :: Hierba -> Raton -> Raton
aplicarHierba hierba = hierba

aplicarMedicamento :: Medicamento -> Raton -> Raton
aplicarMedicamento medicamento raton = foldl (flip aplicarHierba) raton medicamento
----------------------------------------------------------------------------------------------------------------------------------------------
pondsAntiAge :: Medicamento
pondsAntiAge = [hierbaBuena, hierbaBuena, hierbaBuena, alcachofa]
----------------------------------------------------------------------------------------------------------------------------------------------
type Potencia = Number
reduceFatFast :: Potencia -> Medicamento
reduceFatFast potencia = [hierbaVerde "obesidad"] ++ replicate potencia alcachofa 
----------------------------------------------------------------------------------------------------------------------------------------------
pdepCilina :: Medicamento
pdepCilina = map hierbaVerde sufijosInfecciosas

sufijosInfecciosas = ["sis", "itis", "emia", "cocos"]

-- PUNTO 4 
type Condicion = Number -> Bool

cantidadIdeal :: Condicion -> Number
cantidadIdeal condicion =  head (filter condicion listaDeNaturales) 

listaDeNaturales :: [Number]
listaDeNaturales = [1..]
----------------------------------------------------------------------------------------------------------------------------------------------
lograEstabilizar :: Medicamento -> [Raton] -> Bool
lograEstabilizar medicamento comunidad = all ratonEstabiliza (aplicarMedicamentoAComunidad medicamento comunidad)

aplicarMedicamentoAComunidad :: Medicamento -> [Raton] -> [Raton]
aplicarMedicamentoAComunidad medicamento = map (aplicarMedicamento medicamento) 

ratonEstabiliza :: Raton -> Bool
ratonEstabiliza raton = tieneMenosDe3Enfermedades raton &&  noTieneSobrepeso raton

tieneMenosDe3Enfermedades :: Raton -> Bool
tieneMenosDe3Enfermedades = (<3) . length . enfermedades

noTieneSobrepeso :: Raton -> Bool
noTieneSobrepeso = ((<=1) . peso)
----------------------------------------------------------------------------------------------------------------------------------------------
potenciaIdeal :: [Raton] -> Potencia
potenciaIdeal comunidad = cantidadIdeal (esPotenciaIdeal comunidad)

esPotenciaIdeal :: [Raton] -> Potencia -> Bool
esPotenciaIdeal comunidad potencia = lograEstabilizar (reduceFatFast potencia) comunidad

{- 5) Queremos saber si un medicamento logra estabilizar una comunidad infinita. ??Podemos saberlo? Responder en estos dos casos:
a) Si todos los ratones quedan con menos de 1kg y sin enfermedades. Justificar.
b) Si un rat??n queda con 2kg y 4 enfermedades. Justificar.

6) Responder en base al ejercicio
a) ??Qu?? cambios deber??a hacer para agregar una nueva hierba y construir un medicamento con ella? ??Habr??a que modificar las funciones existentes? 
b) ??Qu?? concepto est?? involucrado en la pregunta anterior? ??Para qu?? sirve en este caso?
c) Si se cambia el modelo del rat??n, por ejemplo, ahora queremos que se registre el peso de un rat??n siempre en libras. ??Qu?? funciones habr??a que modificar? -}

-- 5A) No se puede saber, ya que ratonEstabiliza devolver??a todo el tiempo True, pero no sabe si en alg??n momento va a haber alg??n False, ni tampoco sabe
-- que van a ser todos True, por la tanto queda colgado infinitamente.

-- 5B) Si se puede saber, ya que al haber al menos uno que no cumpla ratonEstabiliza, por lazy evaluation, lograEstabilizar devolver??a False sin importarle
-- lo que sigue.

-- 6A) Para construir una nueva hierba simplemente habr??a que hacerla, sin modificar ninguna funci??n. Y para construir un medicamento, en caso de que ya exista
-- y lo que se quiere hacer es agregarle esta hierba a ese medicamento, simplemente se agrega la hierba a la lista. En caso de que se quiera hacer uno
-- completamente nuevo, solamente habr??a que hacerlo, sin modificar otras funciones. 

-- 6B) 
{- - Primero hablar??a sobre el modelado, diciendo que las hierbas est??n modeladas como funciones de tipo Raton -> Raton, lo que permite agregar cualquier hierba 
simplemente definiendo una nueva funci??n, lo que no afecta al resto. Los medicamentos son una lista de esas hierbas, por lo que tambi??n est??n definidos independientes 
uno de los otros. (Creo que esta es la respuesta que m??s concreta a la pregunta del parcial, la forma del modelado, especialmente con funciones.)

- Pero para m?? el concepto clave que te deja hacer todo esto es el orden superior, o sea el poder definir el algoritmo de administrarMedicamento independiente de los 
medicamentos y hierbas que existan en el sistema. Es en c??mo se usan esos modelos donde se saca jugo a lo que dice arriba, lo que tiene sentido, ya que definir un modelo 
independiente y extensible pero que despu??s no resuelva el problema no tiene sentido, siempre hay que tener ese foco. -}

-- 6C) Habr??a que modificar aquellas funciones en las que se utiliza un valor del peso en kilos como condici??n, por ejemplo en alcachofa la condicion es que
-- pese menos de 2 KILOS para hacer algo. En ese caso habr??a que modificarlo por su equivalente en libras y nada m??s. El resto de funciones que modifican el 
-- peso del raton y trabajan con el peso, siguen siendo las mismas, solo que trabajar??an en libras.


