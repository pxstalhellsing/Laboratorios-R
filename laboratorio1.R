# Laboratorio 1 - Pia Fuentes Mellado
# Fecha: 05-08-2026
curso <- "Probabilidad y Estadistica Computacional"
semana <- 2
curso
semana

set.seed(2026)
moneda_50 <- sample(
  c("Cara", "Sello"),
  size = 50,
  replace = TRUE
)

frecuencias_50 <- table(moneda_50)
proporciones_50 <- prop.table(frecuencias_50)

frecuencias_50
proporciones_50

barplot(frecuencias_50,
        col = c("#247B7B", "#8B2942"),
        main = "Resultados en 50 lanzamientos",
        ylab = "Frecuencia")


set.seed(2026)
moneda_1000 <- sample(c("Cara", "Sello"), size = 1000, replace = TRUE)
frecuencias_1000 <- table(moneda_1000)
proporciones_1000 <- prop.table(frecuencias_1000)
frecuencias_1000
proporciones_1000

set.seed(310)
dado <- sample(1:6, size = 120, replace = TRUE)
frecuencias_dado <- table(dado)
prop.table(frecuencias_dado)
evento_A <- dado %in% c(2, 4, 6)
evento_B <- dado > 4
fr_A <- mean(evento_A)
fr_B <- mean(evento_B)
fr_union <- mean(evento_A | evento_B)
fr_interseccion <- mean(evento_A & evento_B)
fr_A
fr_B
fr_union
fr_interseccion


set.seed(811)
estado <- sample(
  c("Correcta", "Lenta", "Falla"),
  size = 500,
  replace = TRUE,
  prob = c(0.75, 0.20, 0.05)
)
tabla_estado <- table(estado)
prop_estado <- prop.table(tabla_estado)
tabla_estado
round(prop_estado, 3)
barplot(tabla_estado,
        col = c("#247B7B", "#F0B44D", "#8B2942"),
        main = "Estado de 500 solicitudes",
        ylab = "Frecuencia")

