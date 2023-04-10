
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
#H0: El peso medio de las manzanas es de 110g, mu = 110g
#HA: El peso medio de las manzanas es menor a 110g, mu < 110g

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
grafico <- ggplot(distr.normal, aes(x,densidad.prob)) + geom_line(color="blue") + ylab("")
grafico <- grafico + geom_area(data = subset(data.frame(x,densidad.prob), x < 108),
                         aes(y=densidad.prob),
                         colour = "red",
                         fill = "red",
                         alpha = 0.3)
print(grafico)

#Por tanto la probabilidad de que el agricultor cometa un error de tipo I
#está dado por el valor de significancia que es igual a la probalidad de
#tomar un valor dentro del area de rechazo (area roja)

prob.menor.108 <- pnorm(118, mean = valor.nulo, sd= err.est, lower.tail = TRUE)
print(prob.menor.108)

cat("La probabilidad de cometer un error de Tipo I es de: ", prob.menor.108, "% \n\n")


