# Ejercicio N°7

# Grupo número 1 
# Integrante Jaime Alejandro Carrasco Quintrequeo 
# Integrante Matías Andrés Colil Colil 
# Integrante Matías Ignacio Hernández Canales

#Pregunta 1

#Una joven emprendedora que crea y comercializa amigurumis desea analizar el interés que generan sus dos 
#líneas de productos: Harry Potter y The Lord of the Rings, entre niños y adolescentes. Para ello, ha 
#encuestado a 12 niños y 9 adolescentes. 5 de los primeros y 6 de los segundos prefirieron la línea de Harry 
#Potter, mientras que los restantes optaron por la de The Lord of the Rings. ¿Influye el rango etario en la 
#preferencia por Harry Potter?

#       Interes
# Producto	    HP	TLOTR
# Niños 	      6   3
# Adolescentes  5   6

#Se puede observar en la tabla tiene campos con 5 o menos observaciones
#ademas son variables dicotomicas e independientes, por tanto se puede
#utilizar la prueba exacta de fisher

#Hipotesis:
#H0: El rango etario no influye en la preferencia por Harry Potter
#HA: El rango etario si influye en la preferencia por Harry Potter

niños <- c(6,3)
adolescentes <- c(5,6)
tabla<- rbind(niños,adolescentes)
colnames(tabla)<-c("Harry potter", "The Lord of the Rings")
print(tabla)

prueba.p1<- fisher.test(tabla,conf.level = 0.95)
print(prueba.p1)

#Dado el p-valor = 0.4059 se rechaza la hiptesis nula en favor de la hipotesis alternativa
##De esta forma podemos concluir con un 95% de confianza que el rango etario si influye en la preferencia por Harry Potter

#Pregunta 2

#El gerente comercial de una importante plataforma de streaming está seguro de que a el interés por una 
#serie de Star Trek cambió muchísimo durante la emisión de la segunda temporada. Sin embargo, ha 
#preguntado a 20 personas si les ha gustado cada una de las temporadas, con el siguiente resultado:
#▪ A 3 personas les gustan ambas temporadas.
#▪ A 1 personas solo les gusta la primera temporada.
#▪ A 10 personas solo les gusta la segunda temporada.
#▪ A 6 personas no les gusta ninguna temporada.
#¿Soportan estos datos la creencia del gerente comercial?

#Hipotesis
#H0: El interes por la serie Star Trek no cambio durante la emision de la segunda temporada
#HA: El interes por la serie Star Trek si cambio durante la emision de la segunda temporada

#Tabla
#Se puede observar en la tabla tiene campos con 5 o menos observaciones
#ademas son variables dicotomicas e independientes, por tanto se puede
#utilizar la prueba exacta de fisher

tabla.p2 <- matrix(c(1, 10, 10, 1, 3, 6), nrow = 2)
colnames(tabla.p2) <- c("Temporada 1", "Temporada 2","Ambas temporadas")
rownames(tabla.p2) <- c("Interes", "Sin interes")

print(tabla.p2)

prueba.p2<-fisher.test(tabla.p2,conf.level = 0.95)
print(prueba.p2)

#Dado el p-valor = 0.000306 se rechaza la hiptesis nula en favor de la hipotesis alternativa
#De esta forma podemos concluir con un 95% de confianza que el interes por la serie Star Trek
#