library(tidyverse) 
library(RVAideMemoire) 
library(rcompanion)
library(dplyr)

# Ejercicio N°7
# Grupo número 1 
# Integrante Jaime Alejandro Carrasco Quintrequeo 
# Integrante Matías Andrés Colil Colil 
# Integrante Matías Ignacio Hernández Canales

# Pregunta 1

# Una joven emprendedora que crea y comercializa amigurumis desea analizar el interés que generan sus dos 
# líneas de productos: Harry Potter y The Lord of the Rings, entre niños y adolescentes. Para ello, ha 
# encuestado a 12 niños y 9 adolescentes. 5 de los primeros y 6 de los segundos prefirieron la línea de Harry 
# Potter, mientras que los restantes optaron por la de The Lord of the Rings. ¿Influye el rango etario en la 
# preferencia por Harry Potter?

#       Interes
# Producto	    HP	TLOTR
# Niños 	      6   3
# Adolescentes  5   6

# Se puede observar en la tabla tiene campos con 5 o menos observaciones
# ademas son variables dicotomicas e independientes, por tanto se puede
# utilizar la prueba exacta de fisher

# Hipotesis:
# H0: El rango etario no influye en la preferencia por Harry Potter
# HA: El rango etario si influye en la preferencia por Harry Potter

niños <- c(6,3)
adolescentes <- c(5,6)
tabla<- rbind(niños,adolescentes)
colnames(tabla)<-c("Harry potter", "The Lord of the Rings")
print(tabla)

prueba.p1<- fisher.test(tabla,conf.level = 0.95)
print(prueba.p1)

# Dado el p-valor = 0.4059 se rechaza la hiptesis nula en favor de la hipotesis alternativa
# De esta forma podemos concluir con un 95% de confianza que el rango etario si influye en la preferencia por Harry Potter

# Pregunta 2

# El gerente comercial de una importante plataforma de streaming está seguro de que a el interés por una 
# serie de Star Trek cambió muchísimo durante la emisión de la segunda temporada. Sin embargo, ha 
# preguntado a 20 personas si les ha gustado cada una de las temporadas, con el siguiente resultado:
# ▪ A 3 personas les gustan ambas temporadas.
# ▪ A 1 personas solo les gusta la primera temporada.
# ▪ A 10 personas solo les gusta la segunda temporada.
# ▪ A 6 personas no les gusta ninguna temporada.
# ¿Soportan estos datos la creencia del gerente comercial?

# Hipotesis
# H0: El interes por la serie Star Trek no cambio durante la emision de la segunda temporada
# HA: El interes por la serie Star Trek si cambio durante la emision de la segunda temporada

# Tabla
# Se puede observar en la tabla tiene campos con 5 o menos observaciones
# ademas son variables dicotomicas e independientes, por tanto se puede
# utilizar la prueba exacta de fisher

tabla.p2 <- matrix(c(1, 10, 10, 1, 3, 6), nrow = 2)
colnames(tabla.p2) <- c("Temporada 1", "Temporada 2","Ambas temporadas")
rownames(tabla.p2) <- c("Interes", "Sin interes")

print(tabla.p2)

prueba.p2<-fisher.test(tabla.p2,conf.level = 0.95)
print(prueba.p2)

# Dado el p-valor = 0.000306 se rechaza la hiptesis nula en favor de la hipotesis alternativa
# De esta forma podemos concluir con un 95% de confianza que el interes por la serie Star Trek


# Pregunta 3
# Minerva McGonagall, actual directora del Colegio Hogwarts de Magia y Hechicería, está haciendo 
# un seguimiento de las actividades laborales y académicas de los egresados más recientes del colegio.
# Desea saber si la casa a la que pertenecían los egresados está relacionada con un posterior empleo 
# en el Ministerio de Magia. ¿Qué puede inferir a partir de los siguientes datos?
#            Ravenclaw Slytherin Hufflepuff Gryffindor 
# Ministerio 45        44        21         31 
# Otro       91        95        87         134 

# Para esta pregunta se utilizara la prueba xi cuadrado de pearson de homogeneidad con un nivel de
# confianza p < 0.05:
# H0 = No existe relacion entre la casa de estudios y el posterior empleo en el ministerio de magia.
# HA = Existe relacion entre la casa de estudios y el posterior empleo en el ministerio de magia.

ministerio <- c(45, 44, 21, 31)
otro <- c(91, 95, 87, 134)
tabla_hogwards <- matrix(c(ministerio, otro), nrow = 2)
rownames(tabla_hogwards) <- c("Ministerio", "Otro")
colnames(tabla_hogwards) <- c("Ravenclaw", "Slytherin", "Hufflepuff", "Gryffindor")
print(tabla_hogwards)
resultado <- chisq.test(tabla_hogwards)
print(resultado)

# Dado que p = 0.1379 se falla al rechazar la hipotesis nula, por lo que podemos concluir con un
# que no existe relacion entre la casa de estudios y el posterior empleo en el ministerio de magia.


# Pregunta 4
# El Departamento de Educación Mágica del Ministerio de Magia desea saber si existen diferencias 
# significativas en el desempeño de los estudiantes del Colegio Hogwarts de Magia y Hechicería en 
# los TIMOS de asignaturas esenciales: Pociones, Defensa Contra las Artes Oscuras, Transformaciones 
# y Encantamientos. Para ello, le ha entregado un archivo de datos que, para dichas asignaturas, 
# indica si cada estudiante registrado obtuvo una Matrícula de Honor en Brujería (MHB) o falló (F). 
# ¿Qué puede concluir el encargado del Departamento de Educación Mágica? Indicación: obtenga, a partir
# del archivo EP07 Datos.csv, una muestra de 100 estudiantes usando la semilla 1983.

# Se usara la prueba Q de Cochran
datos <- read.csv2(file.choose(), stringsAsFactors = TRUE)

# H0 = El desempeño es el mismo para las asignaturas esenciales
# H1 = El desempeño es diferente para las asignaturas esenciales
set.seed(1983)
matriz <- data.matrix(datos)
matriz[matriz == 1] <- 0
matriz[matriz == 2] <- 1
datos <- as.data.frame(matriz)
muestra <- datos[sample(nrow(datos), 100), ]

muestra <- muestra %>% pivot_longer(c("Pociones","Defensa","Transformaciones", "Encantamientos"), 
                              names_to="Asignaturas", 
                              values_to="resultado")

muestra[["Id"]]<-factor(muestra[["Id"]]) 
muestra[["Asignaturas"]]<-factor(muestra[["Asignaturas"]])

#Hacer prueba Q de Cochran. 
prueba<-cochran.qtest(resultado~Asignaturas|Id, 
                      data=muestra,alpha=0.05)

print(prueba)

# Dado que p<alpha, se rechaza la hipótesis nula en favor de la alternativa,
# con un 95% de confianza. Es decir, el desempeño es diferente para las asignaturas escenciales.
