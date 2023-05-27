if (!require ( pwr )) {
  install.packages ("pwr" , dependencies=TRUE)
  require (pwr)
}
library(dplyr)
library(ggpubr)
library(pwr)
# Grupo número 1
# Integrante Jaime Alejandro Carrasco Quintrequeo
# Integrante Matías Andrés Colil Colil
# Integrante Matías Ignacio Hernández Canales

#Pregunta 1
#El agricultor está seguro de que el verdadero peso medio no puede ser superior a 110 gramos y piensa
#rechazar la hipótesis nula cuando la muestra presente un peso medio menor a 108 gramos. Determine, 
#usando herramientas gráficas, la probabilidad de que cometa un error de tipo I.

#se pide encontrar la probabilidad de que el agricultor cometa un eror de tipo I 
#y esto significa que se debe encontrar el valor de significancia alfa
#que es el que determina la probalidad de cometer un error de tipo I

#Hipotesis:
#H0: El peso medio de las manzanas es de 110g, mu >= 110g (hipotesis nula)
#HA: El peso medio de las manzanas es menor a 110g, mu < 110g (hipotesis alternativa)

#Valores conocidos
n<-300 #tamaño de la muestra
des.est<- 15 #desviacion estandar
err.est<- des.est / sqrt(n) #error estandar

#generamos una distribucion normal en torno al valor nulo de la hipotesis
#y usamos 1000 valores para generar la secuencia
valor.nulo = 110
puntos <- 1000

x <- seq(valor.nulo-5, valor.nulo+5,length.out = puntos)
densidad.prob <- dnorm(x, mean = valor.nulo, sd = err.est)
distr.normal <-data.frame(x,densidad.prob)
grafico <- ggplot(distr.normal, aes(x))
grafico <- grafico + stat_function(fun = dnorm,
                                 args = list(mean = valor.nulo,
                                             sd = err.est),
                                 colour = "blue", size = 1)
grafico <- grafico + geom_area(data = subset(distr.normal, x < 108),
                         aes(y=densidad.prob),
                         colour = "red",
                         fill = "red",
                         alpha = 0.5) + ylab("") + scale_y_continuous(breaks = NULL)
print(grafico)

#Por tanto la probabilidad de que el agricultor cometa un error de tipo I
#está dado por el valor de significancia que es igual a la probalidad de
#tomar un valor dentro del area de rechazo (area roja)

prob.error.i <- pnorm(108, mean = valor.nulo, sd= err.est, lower.tail = TRUE)
print(prob.error.i)

cat("La probabilidad de cometer un error de Tipo I es de: ", prob.error.i, "% \n\n")

#Pregunta 2
#Suponga ahora que el verdadero peso medio de las manzanas es de 109,5 gramos. Determine mediante 
#herramientas gráficas cuál sería la probabilidad de que el agricultor, quien obviamente no conoce este dato, 
#cometa un error de tipo II.


#Se nos pide encontrar la probabilidad de que el agricultor cometa un error de tipo II

#Valores conocidos
media_verdadera<-109.5

#Tenemos que superponer el grafico de la hiptesis nula con este nuevo grafico para 
#ver de manera grafica la probabilidad del error de tipo II
#Grafico con la media verdadera

x2 <- seq(media_verdadera-5, media_verdadera+5,length.out = puntos)
densidad.prob2 <- dnorm(x2, mean = media_verdadera, sd = err.est)
distr.normal2 <-data.frame(x = x2, y = densidad.prob2)

grafico2 <- grafico + stat_function(fun = dnorm,
                                    args = list(mean = media_verdadera, sd = err.est),
                                    colour = "red", size = 1)
print(grafico2)
grafico2<- grafico2 + geom_area(data = subset(distr.normal2, x >= 108),
                                aes(y=y),
                                colour = "blue",
                                fill = "blue",
                                alpha = 0.3)
print(grafico2)

#Por tanto la probabilidad de que el agricultor cometa un error de tipo II
#está dado por el valor de significancia que es igual a la probalidad de no
#tomar un valor dentro del area de rechazo (area azul)

beta <- pnorm(108, mean = media_verdadera, sd= err.est, lower.tail = FALSE)

print(beta)

cat("La probabilidad de cometer un error de Tipo II es de: ", beta, "% \n\n")

# Pregunta 3
# Teniendo en cuenta que en realidad no se conoce el verdadero peso medio, genere ahora un gráfico del
# poder teniendo en cuenta que el agricultor piensa rechazar la hipótesis nula si la muestra presenta un peso
# medio menor a 109 gramos, pero suponiendo ahora que el peso volumen medio podría variar entre 108 y
# 110 gramos.

#x3 <- seq(108, 110,length.out = 300)
#Preguntar al mati pq 1000 y no 300, no sale en los comentarios el pq, en el enunciado dice 300.}

# Generar la secuencia aleatoria de números
x3 <- runif(300, min = 108, max = 110)

# Ajustar la media y la desviación estándar
x3 <- (x3 - mean(x3)) / sd(x3) * 15 + mean(x3)


media3 <- mean(x3)
# poder3=pwr.norm.test(n=300,
#                     delta=x3,
#                     sd=15,
#                     sig.level=0.05,
#                     type="one.sample",
#                     alternative = "two.sided")$power
# poder3 = pwr.norm.test(d=((media3-109)/des.est), sig.level =0.05, power = 0.8, alternative = "less")
poder3 <- power.t.test(n=x3, 
                       delta=media3-109, 
                       sd=des.est,
                       sig.level = 0.05, 
                       type = "one.sample", 
                       alternative = "two.sided" )$power

pow3 <- data.frame(x3,poder3)

g3 <- ggplot(pow3,aes(x3,poder3))
g3 <- g3 + geom_line ()
g3 <- g3 + labs ( colour = "red" )
g3 <- g3 + ylab ( " Poder estad í stico " )
g3 <- g3 + xlab ( " Tama ñ o del efecto " )

print(g3)


#Pregunta 4
#Considerando un peso medio verdadero de 109 gramos, calcule usando funciones de R 
#(o alguno de sus paquetes) cuántas manzanas deberían revisarse para conseguir un 
#poder estadístico de 0,75 y un nivel de significación de 0,05.

#Para poder obtener los datos estadisticos que se piden vamos a utilizar la
#funcion power.t.test en la cual si dejamos el valor de n como NULL
#nos entregara el tamaño de la muestra

media_p4<- 109


res <- pwr.norm.test(d = ((media_p4 - valor.nulo)/des.est), sig.level = 0.05,
                                   power = 0.75, alternative = "less")
tamaño.test.z <- ceiling(res[["n"]])

cat("El tamaño de la muestra para los valores estadisticos dados es de: ", tamaño.test.z, " \n\n")


#Pregunta 5
# Repita el ejercicio de la pregunta anterior, suponiendo ahora que el agricultor es muy exigente y desea
# reducir la probabilidad de cometer un error de tipo I a un 1% solamente?

media_p4<- 109


res <- pwr.norm.test(d = ((media_p4 - valor.nulo)/des.est), sig.level = 0.01,
                     power = 0.75, alternative = "less")
tamaño.test.z <- ceiling(res[["n"]])

cat("El tamaño de la muestra para los valores estadisticos dados es de: ", tamaño.test.z, " \n\n")