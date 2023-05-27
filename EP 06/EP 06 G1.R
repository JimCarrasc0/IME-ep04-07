# Ejercicio N°6

# Grupo número 1 
# Integrante Jaime Alejandro Carrasco Quintrequeo 
# Integrante Matías Andrés Colil Colil 
# Integrante Matías Ignacio Hernández Canales 
#Importación de paquetes


datos <- data.frame(
  Nivel_de_fanatismo = factor(c("Fanático extremo","Fanático moderado","No fanático")),
  Hombres = c(422,502,396),
  Mujeres = c(158,291,431)
)

#Pregunta 1
# Estudios previos habían determinado más de 30% de las mujeres encuestadas son fanáticas moderadas. 
#¿Respaldan estos datos tal estimación?


#inferencia de una proporcion con muestra, se puede ocupar el metodo de Wilson o Wald

#Formular hipotesis

#H0 : Proporción de mujeres que son fanáticas moderadas más del 30% (p=0.3)
#HA : Proporción de mujeres que son fanáticas moderadas es menos del 30% (p>0.3)

#Datos

valor.nulo <- 0.3
mujeres.moderada <-  datos[["Mujeres"]][2] 
mujeres.no_moderada <- datos[["Mujeres"]][1] + datos[["Mujeres"]][3]
n <- mujeres.moderada + mujeres.no_moderada


#Verificamos si se cumplen las condiciones iniciales para poder utilizar el metodo de Wilson o Wald
#Observaciones independientes y condicion de exito-fracaso
#Para el caso de las observaciones, supones que son independientes
#Ahora se procede a verificar la condicion de exito-fracaso

exitos.esperados <- n * valor.nulo
fracasos.esperados <- n * (1-valor.nulo)

cat("\nCasos donde las mujeres son fanáticas moderadas: ",exitos.esperados,"\nCasos donde las mujeres no son fanáticas moderadas: ", fracasos.esperados)

#Por tanto verifican las condiciones para poder ocupar el metodo de Wilson
#Procedemos con el metodo de Wilson y fijamos un nivel de significación de 0.05

prueba.p1 <- prop.test(x=mujeres.moderada, n = n, p = valor.nulo,
                       alternative="greater", conf.level = 0.95, correct = FALSE)

print(prueba.p1)

#Dado que el valor p=0.02351 se rechaza la hipotesis nula en favor de la hipotesis alternativa
#De esta forma podemos concluir con un 95% de confianza que más del 30% de las mujeres son fanáticas moderadas de Star Wars


#Pregunta 2
#Según estos datos, ¿es menor la proporción de fanáticos extremos entre mujeres que entre hombres?

#Inferencia sobre dos proporciones por tanto podemos seguir ocupando Wilson

#Hipotesis
#Hipotesis Nula: La proporción de fanáticos extremos es menor en mujeres que en hombres (p1 - p2 = 0)
#Hipotesis Alternativa: la proporción de fanáticos extremos es mayor en mujeres que en hombres (p1 - p2 < 0)

mujeres.extremos <- datos[["Mujeres"]][1]
hombres.extremos <-datos[["Hombres"]][1]
n.extremos <- datos[["Hombres"]][1] + datos[["Mujeres"]][1]
n.h <- datos[["Hombres"]][1] +  datos[["Hombres"]][2] + datos[["Hombres"]][3]


#Verificamos si se cumplen las condiciones iniciales para poder utilizar el metodo de Wilson o Wald
#Observaciones independientes y condicion de exito-fracaso
#Para el caso de las observaciones, supones que son independientes
#Ahora se procede a verificar la condicion de exito-fracaso
prop_hombres <- prop.table(datos$Hombres)[1]
prop_mujeres <- prop.table(datos$Mujeres)[1]
valor.nulo <- prop_hombres

exitos.extremos <- n.extremos * valor.nulo
fracasos.extremos <- n.extremos * (1-valor.nulo)


cat("\nExitos: ",exitos.extremos,"\nFracasos: ", fracasos.extremos)

#Por tanto verifican las condiciones para poder ocupar el metodo de Wilson
#Procedemos con el metodo de Wilson y fijamos un nivel de significación de 0.05

prueba.p2 <- prop.test(x = c(mujeres.extremos, hombres.extremos),
                      n = c(n, n.h),
                      alternative = "less", conf.level = 0.95,
                      correct = FALSE)

print(prueba.p2)

#Dado el p-valor = 1.344e-13 se rechaza la hipotesis nula en favor de la hipotesis alternativa
#por tanto con un 95% de confianza podemos afirmar que la proporcion de mujeres que son fanaticas extremas 
#es menor que la proporcion de hombres que son fanaticos extremos

# Pregunta 3
# Existe la creencia de que hay más fanáticos extremos entre los hombres que entre las mujeres y que dicha
# diferencia supera el 25%. ¿A cuántas personas (hombres y mujeres) se debería encuestar para obtener un
# intervalo de confianza del 95% y poder estadístico de 95%, si se intenta mantener aproximadamente la
# misma proporción de gente estudiada en cada caso?

library(pwr)

diferencia_minima <- 0.25

# Nivel de confianza (95%)
confianza <- 0.95

# Poder estadístico (95%)
poder <- 0.95

# Cálculo del tamaño de muestra necesario
muestra <- power.prop.test(p1 = prop_hombres, p2 = prop_hombres + diferencia_minima, 
                           power = poder, sig.level = 1 - confianza,
                           alternative = "two.sided")$n

cat("La cantidad mínima es de:", ceiling(muestra), "personas\n")
