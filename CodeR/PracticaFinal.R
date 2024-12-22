# Cargar librerías
library(ggplot2)

# Establecer el directorio de trabajo al directorio del archivo .R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Función para cargar los datos
load_data <- function(file_path) {
  df <- read.csv(file_path) 
  View(data)
  print(head(df))
  # Mostrar el resumen de los datos
  summary_statistics(df)
  return(df)
}

# Función para visualizar la distribución de temperaturas en Fahrenheit
plot_temperature_distribution_Fahrenheit <- function(df) {
  # Crear la gráfica y devolverla para ser mostrada
  q <- ggplot(df, aes(x = AvgTemperature)) +
    geom_histogram(bins = 30, fill = 'red', alpha = 0.7) +
    theme_minimal() +
    ggtitle('Distribución de Temperaturas en Fahrenheit')
  
  # Devolver el objeto de la gráfica para que pueda ser visualizado
  print(q)
}

# Función para limpiar los datos
clean_data <- function(df) {
  # Reemplazar -99 con NA para indicar datos faltantes
  df$AvgTemperature[df$AvgTemperature == -99] <- NA
  
  # Verificar el número de filas antes de la limpieza
  total_filas_anterior <- nrow(df)
  na_count_before <- sum(is.na(df))
  
  # Eliminar filas con NA
  df <- na.omit(df)
  
  # Verificar el número de filas después de la limpieza
  total_filas_despues <- nrow(df)
  na_count_after <- sum(is.na(df))
  
  # Mostrar resultados de la limpieza
  cat("Total de filas antes de la limpieza:", total_filas_anterior, "\n")
  cat("Total de filas después de la limpieza:", total_filas_despues, "\n")
  cat("Cantidad de NA eliminados:", total_filas_anterior - total_filas_despues, "\n")
  cat("Total de NAs en el dataset antes de la limpieza:", na_count_before, "\n")
  cat("Total de NAs en el dataset después de la limpieza:", na_count_after, "\n")
  
  return(df)  # Retorna el dataframe limpio
}

# Función para convertir Fahrenheit a Celsius
convert_to_celsius <- function(df) {
  df$AvgTemperature_Celsius <- (df$AvgTemperature - 32) * 5 / 9
  return(df)
}


# Función para visualizar la distribución de temperaturas en Celsius
plot_temperature_distribution_Celsius <- function(df) {
  # Crear la gráfica y devolverla para ser mostrada
  p <- ggplot(df, aes(x = AvgTemperature_Celsius)) +
    geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
    theme_minimal() +
    ggtitle('Distribución de Temperaturas en Celsius')
  
  # Devolver el objeto de la gráfica para que pueda ser visualizado
  print(p)
}

# Función para mostrar el resumen de los datos
summary_statistics <- function(df) {
  print(summary(df))
}

# Función principal para ejecutar todo el proceso
main <- function(file_path) {
  
  # Cargar los datos
  df <- load_data(file_path)
  
  # Mostrar la distribución de temperaturas en Fahrenheit
  plot_temperature_distribution_Fahrenheit(df)
  
  # Limpiar los datos
  df <- clean_data(df)
  
  # Convertir a Celsius
  df <- convert_to_celsius(df)
  
  # Verificar las primeras filas de la conversión
  print(head(df[, c("AvgTemperature", "AvgTemperature_Celsius")]))
  
  # Mostrar la distribución de temperaturas en Celsius
  plot_temperature_distribution_Celsius(df)
  
  # Mostrar el resumen de los datos
  summary_statistics(df)
  
}

# Ejecutar el script (cambiar la ruta del archivo según corresponda)
main("../data/city_temperature.csv")
