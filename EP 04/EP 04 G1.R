
library(dplyr)
library(ggpubr)
# Grupo número 1
# Integrante Jaime Alejandro Carrasco Quintrequeo
# Integrante Matías Andrés Colil Colil
# Integrante Matías Ignacio Hernández Canales

# Importación de datos desde un archivo .csv con formato español
# stringsAsFactors = TRUE para la conversión de variables inmediata
pollitos <- read.csv2(file.choose(), stringsAsFactors = TRUE)

# 1. El criador a cargo del estudio cree que el peso medio de los pollitos alimentados con habas, a los 6 días de
# nacidos, es de 75,2 gramos. ¿Soportan los datos esta afirmación?

polloPeso_Haba6 <- pollitos[pollitos$DIETA == "Habas" & pollitos$DIA == "6",][["PESO"]]
print(polloPeso_Haba6)
tamaño<- length(polloPeso_Haba6)
print(tamaño)

# Como son solo 10 pollos los que se tienen para la prueba, se utilizará la prueba T de student
# Verificamos si la muestra cumple las condiciones para poder aplicar la prueba T de student
# Por tanto, como la muestra se trata de diferentes pollitos, se puede asumir que las observaciones
# son independientes. Se tiene también que verificar si las observaciones siguen una distribución normal, por lo tanto
# podemos utilizar shapiro-wilk para poder verificar esto

normal<- shapiro.test(polloPeso_Haba6)
print(normal)

# Dado que el valor W del test Shapiro-Wilkinson se puede afirmar que las observaciones siguen una distribución normal
# Fijamos el nivel de significación
significacion <- 0.05

# Formulamos la hipótesis
# H0: Los pollos alimentados con habas, a los 6 días PESO == 75,2 g
# HA: los pollos alimentados con habas, a los 6 días PESO != 75,2 g

h0 <- 75.2

prueba.t <- t.test(polloPeso_Haba6, alternative = "two.sided", mu = h0, conf.level = 1 - significacion)

print(prueba.t)

# Dado que el p-valor obtenido es mayor que el nivel de significación,se concluye que se falla al rechazar la hipótesis nula
# Por lo tanto se puede inferir con un 95% de confianza que el peso promedio de los pollitos no es distinto a 75.2 g



# 2. ¿Sugieren los datos que, en promedio, los pollitos alimentados con soya aumentan su peso en más de 169,5
# gramos a 16 días desde su nacimiento?

# Como son solo 10 pollos los que se tienen para la prueba, se utilizará la prueba T de student
# Verificamos si la muestra cumple las condiciones para poder aplicar la prueba T de student
# Por tanto, como la muestra se trata de diferentes pollitos, se puede asumir que las observaciones
# son independientes. Se tiene también que verificar si las observaciones siguen una distribución normal, por lo tanto
# podemos utilizar shapiro-wilk para poder verificar esto

polloPeso_Soya16 <- pollitos[pollitos$DIETA == "Soya" & pollitos$DIA == "16",][["PESO"]]
tamaño16<- length(polloPeso_Soya16)

polloPeso_Soya0 <- pollitos[pollitos$DIETA == "Soya" & pollitos$DIA == "0",][["PESO"]]
tamaño0<- length(polloPeso_Soya0)

diferencia = polloPeso_Soya16 - polloPeso_Soya0
normalidad <- shapiro.test( diferencia )
print( normalidad )
print(diferencia)
# Como p-value > alfa sigue distribución normal, entonces se continua con la prueba
# t de student

# Fijar un nivel de significación.
alfa <- 0.05

# Hipotesis:
# H0:Los pollos alimentados con soya a los 16 días tendrán aumento de PESO >= 169,5
# HA:Los pollos alimentados con soya a los 16 días tendrán aumento de PESO < 169,5

h0 <- 169.5

prueba_t16 <- t.test ( diferencia ,
                          alternative = "less",
                          mu = h0 ,
                          conf.level = 1 - alfa )
print (prueba_t16)

# Como la media de la diferencias esta dentro del intervalo de  confianza
# y ademas el p-value es mayor que el nivel de significación, entonces se 
# rechaza H0 a favor de HA, por lo tanto se puede inferir con un 95% de confianza 
# que el aumento de peso promedio de los pollitos alimentados con soya es menor a 169,5 g

######################################################################

# 3. ¿Es posible afirmar que, en promedio, los pollitos alimentados con habas superan por menos de 29,03
# gramos a los alimentados con linaza a los 10 días de nacidos?

polloPeso_Haba <- pollitos[pollitos$DIETA == "Habas" & pollitos$DIA == "10",][["PESO"]]
polloPeso_Linaza <- pollitos[pollitos$DIETA == "Linaza" & pollitos$DIA == "10",][["PESO"]]

tamañoHaba<- length(polloPeso_Haba)
tamañoLinaza<- length(polloPeso_Linaza)


# Dado que el tamaño de las muestras son pequeñas podemos utilizar la prueba T de student
# Ahora debemos de comprobar las condiciones para saber si podemos utilizar la prueba

# Como los pollitos han sido alimentados de distinta forma por lo tanto podemos asumir que se tratan
# de muestras independientes

# Aplicamos el test de Shapiro-Wilk para ambas muestras

normalHaba<- shapiro.test(polloPeso_Haba)
print(normalHaba)
normalLinaza<- shapiro.test(polloPeso_Linaza)
print(normalLinaza)

# Observamos en ambas muestras los valores W y P y podemos inferir que los datos si siguen una distribución normal
# así que si podemos aplicar la prueba T de student

# Asignamos la significación
significacion_alimentos <- 0.05

# Formulamos las hipótesis

# H0: Luego de alimentar a los pollitos con habas y linaza durante 10 días, los pollitos alimentados con habas superan en 
# en promedio por 29.03 gramos a los pollitos alimentados con linaza
# μhabas - μlinaza = 29.03

# HA: Luego de alimentar a los pollitos con habas y linaza durante 10 días, los pollitos alimentados con habas superan por menos 
# de 29.03 gramos a los pollitos alimentados con linaza
# μhabas - μlinaza < 29.03

H0_Peso <- 29.03

prueba_t_pesos <- t.test(x = polloPeso_Haba  , y = polloPeso_Linaza, 
                         alternative = "less", mu = H0_Peso, paired = FALSE, conf.level = 1-significacion_alimentos)
print(prueba_t_pesos)

# Dado el resultado de la prueba T de student y dado el p-valor se puede inferir que:
# se rechaza la hipótesis nula a favor de la hipótesis alternativa y podemos decir
# con 95% de confianza que la diferencia entre el promedio de los pollitos alimentados con habas y linaza es 
# menor a 29.03 gramos