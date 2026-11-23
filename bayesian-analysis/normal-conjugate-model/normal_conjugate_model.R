
# Definir los parámetros de la distribución
media <- 10
desviacion <- 2.3

# Generar un rango de valores para x
x <- seq(0, 20, length.out = 1000)

# Calcular la densidad de probabilidad para cada valor de x
y <- dnorm(x, mean = media, sd = desviacion)

# Graficar la función de densidad
plot(x, y, type = "l", col = "blue", lwd = 2,
     xlab = "Miles de kilos por año",
     ylab = "Densidad de probabilidad",
     main = "Distribución de las necesidades mínimas de aceitunas")



# Valores de media y desviación típica proporcionados por los consultores
medias <- c(9.97, 10.26, 11.94, 10.85, 9.58)
desviaciones <- c(1.75, 1.83, 2.05, 1.78, 1.83)

# Definir las necesidades mínimas de miles de kilos por año
media_necesidades <- 10
desviacion_necesidades <- 2.3

# Calcular la probabilidad a priori para cada hipótesis (uniforme)
priori <- rep(1/5, 5)

# Calcular la probabilidad a posteriori para cada hipótesis
verosimilitud <- dnorm(media_necesidades, mean = medias, sd = desviaciones)

producto_verosimilitud_priori <- verosimilitud * priori
denominador <- 0.1842
posteriori <- producto_verosimilitud_priori / denominador

# Mostrar los resultados
resultado <- data.frame(Hipotesis = 1:5, Media = medias, Desviacion = desviaciones, 
                        Priori = priori, Verosimilitud = verosimilitud,
                        Producto_Verosimilitud_Priori = producto_verosimilitud_priori, 
                        Denominador = denominador, Posteriori = posteriori)
resultado


# Especificación de la distribución a priori para la media desconocida
media_priori <- 12
desviacion_priori <- sqrt(0.0625)

# Generar un rango de valores para la media
media_range <- seq(10, 14, length.out = 1000)

# Calcular la densidad de probabilidad para cada valor de media
densidad_priori <- dnorm(media_range, mean = media_priori, sd = desviacion_priori)

# Graficar la distribución a priori
plot(media_range, densidad_priori, type = "l", col = "blue", lwd = 2,
     xlab = "Media de la producción de aceitunas",
     ylab = "Densidad de probabilidad",
     main = "Distribución a priori para la media desconocida")





# Especificación de los datos observados
datos_observados <- 10.2

# Especificación de la distribución de la verosimilitud
media_verosimilitud <- datos_observados
desviacion_verosimilitud <- 0.25

# Función para determinar la verosimilitud
verosimilitud <- function(media) {
  dnorm(datos_observados, mean = media, sd = desviacion_verosimilitud)
}

# Cálculo de la verosimilitud para los datos observados
verosimilitud_datos_observados <- verosimilitud(media_range)


# Especificación de los parámetros
n <- 1  # Tamaño de la muestra
mu_0 <- media_priori
sigma_0_sq <- desviacion_priori^2
mu <- media_verosimilitud
sigma_sq <- desviacion_verosimilitud^2

# Cálculo de la distribución a posteriori para la media desconocida
media_posteriori <- (sigma_sq / (sigma_0_sq + n * sigma_sq)) * mu_0 + 
  (n * sigma_0_sq / (sigma_0_sq + n * sigma_sq)) * datos_observados

# Especificación de la distribución a posteriori para la media desconocida
desviacion_posteriori <- sqrt((sigma_0_sq * sigma_sq) / (sigma_0_sq + n * sigma_sq))

# Generar un rango de valores para la media posteriori
media_posteriori_range <- seq(10, 14, length.out = 1000)

# Calcular la densidad de probabilidad para cada valor de media posteriori
densidad_posteriori <- dnorm(media_posteriori_range, mean = media_posteriori, sd = desviacion_posteriori)

# Graficar la distribución a priori y a posteriori para la media desconocida
plot(media_range, densidad_priori, type = "l", col = "blue", lwd = 2,
     xlab = "Media de la producción de aceitunas",
     ylab = "Densidad de probabilidad",
     main = "Distribución a priori y a posteriori para la media desconocida")
lines(media_posteriori_range, densidad_posteriori, col = "red", lwd = 2)
legend("topright", legend = c("A priori", "A posteriori"), col = c("blue", "red"), lwd = 2)



# Definir función para calcular la distribución a posteriori para la media mediante el uso de conjugados
calcular_posteriori_conjugado <- function(media_priori, desviacion_priori, datos_observados, desviacion_verosimilitud) {
  n <- 1  # Tamaño de la muestra
  mu_0 <- media_priori
  sigma_0_sq <- desviacion_priori^2
  mu <- datos_observados
  sigma_sq <- desviacion_verosimilitud^2
  
  media_posteriori <- (sigma_sq / (sigma_0_sq + n * sigma_sq)) * mu_0 + 
    (n * sigma_0_sq / (sigma_0_sq + n * sigma_sq)) * datos_observados
  
  desviacion_posteriori <- sqrt((sigma_0_sq * sigma_sq) / (sigma_0_sq + n * sigma_sq))
  
  return(list(media = media_posteriori, desviacion = desviacion_posteriori))
}

# Especificación de los parámetros
media_priori <- 12
desviacion_priori <- sqrt(0.0625)
datos_observados_1 <- 10.2
datos_observados_2 <- 39
desviacion_verosimilitud <- 0.25

# Calcular distribución a posteriori para el primer caso (observación de 10.2 miles de kilos)
posteriori_1 <- calcular_posteriori_conjugado(media_priori, desviacion_priori, datos_observados_1, desviacion_verosimilitud)

# Calcular distribución a posteriori para el segundo caso (observación de 39 miles de kilos)
posteriori_2 <- calcular_posteriori_conjugado(media_priori, desviacion_priori, datos_observados_2, desviacion_verosimilitud)

# Mostrar los parámetros de las distribuciones a posteriori en ambos casos
print("Distribución a posteriori para una observación de 10.2 miles de kilos:")
print(posteriori_1)
print("Distribución a posteriori para una observación de 39 miles de kilos:")
print(posteriori_2)


