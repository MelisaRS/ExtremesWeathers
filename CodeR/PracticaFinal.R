library(ggplot2)

# Establecer el directorio de trabajo al directorio del archivo .R
print(getwd())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Función para cargar y visualizacion de los datos
load_data <- function(file_path) {
  df <- read.csv(file_path) 
  View(df)
  print("################## DATA INICIAL ##################")
  print(head(df))
  print("################## SUMMARY INICIAL ##################")
  # Mostrar el resumen de los datos
  print(summary(df))
  return(df)
}

exploration_data <- function(df){
  
  print("################## EXPLORACION DE DATOS ##################")
  # Country
  num_countries <- length(unique(df$Country))
  cat("El número de Paises es:", num_countries, "\n")
  
  countries_states(df)
  
  # Analisis de estados
  list_states(df)
}

# Filtrar el dataset para mostrar solo los países de interés
countries_states <- function(df) {
  
  "Esta lista de paises representan los paises que deberian de tener state en nuestro dataset"
  
  # Lista de países que quieres verificar
  paises_interes <- c("Germany", "Argentina", "Australia", "Austria", "Brazil", 
                      "Myanmar", "US", "India", "Malaysia", "Mexico", "Nigeria", "Venezuela")
  
  # Filtrar por los países de interés
  paises_encontrados <- df[df$Country %in% paises_interes, ]
  
  # Obtener los países únicos que están en el dataset
  paises_unicos_encontrados <- unique(paises_encontrados$Country)
  
  # Mostrar los países encontrados
  cat("Países encontrados en el dataset que deberian de tener State:\n")
  print(paises_unicos_encontrados)
}

# Agrupar por State, Country y contar cuántos estados únicos hay
list_states <- function(df) {
  # Filtrar estados no vacíos
  filtered_df <- df[!(is.na(df$State) | df$State == ""), ]
  
  # Seleccionar columnas relevantes y eliminar duplicados
  unique_states <- unique(filtered_df[, c("Country", "State")])
  
  # Contar el número total de estados únicos
  total_states <- nrow(unique_states)
  
  # Obtener la lista de países únicos correspondientes a los estados
  unique_countries <- unique(unique_states$Country)
  
  cat("Número total de estados:", total_states, "\n")
  cat("Países correspondientes de todos los estados:\n")
  print(unique_countries)
}


plot_temperature_distribution_Fahrenheit <- function(df) {
  q <- ggplot(df, aes(x = AvgTemperature)) +
    geom_histogram(bins = 30, fill = 'red', alpha = 0.7) +
    theme_minimal() +
    ggtitle('Distribución de Temperaturas en Fahrenheit')
  print(q)
}

# Función para limpiar los datos
clean_data <- function(df) {
  # Reemplaza -99 con NA para indicar datos faltantes
  df$AvgTemperature[df$AvgTemperature == -99] <- NA
  
  # Verifica el número de filas antes de la limpieza
  total_filas_anterior <- nrow(df)
  na_count_before <- sum(is.na(df))
  
  # Elimina filas con NA
  df <- na.omit(df)
  
  # Verifica el número de filas después de la limpieza
  total_filas_despues <- nrow(df)
  na_count_after <- sum(is.na(df))
  
  print("################## LIMPIEZA DE DATOS ##################")
  # Mostrar resultados de la limpieza
  print("Resultados de la limpieza de NA")
  print("Las filas con valor -99 en la columna AvgTemperature fueron colocados como valores NA")
  print("ya que -99 en esa columna significa la inexistencia de ese valor")
  cat("Total de filas antes de la limpieza:", total_filas_anterior, "\n")
  cat("Total de filas después de la limpieza:", total_filas_despues, "\n")
  cat("Cantidad de NA eliminados:", total_filas_anterior - total_filas_despues, "\n")
  cat("Total de NAs en el dataset antes de la limpieza:", na_count_before, "\n")
  cat("Total de NAs en el dataset después de la limpieza:", na_count_after, "\n")
  
  # Creacion columna Date
  df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"), format = "%Y-%m-%d")
  
  #eliminancion de columna Innecesarias
  df <- df[, !names(df) %in% c("State", "Day", "Month", "Year")]
  print("Columna State, Day, Month y Year eliminadas")
  
  # Convertir a Celsius
  df$AvgTemperature_Celsius <- (df$AvgTemperature - 32) * 5 / 9
  print("columnas convertidas de °F a °C")
  print(head(df[, c("AvgTemperature", "AvgTemperature_Celsius")]))
  
  print("################## NUEVO DATASET ##################")
  print(head(df))
  return(df)
}

plot_temperature_distribution_Celsius <- function(df) {
  p <- ggplot(df, aes(x = AvgTemperature_Celsius)) +
    geom_histogram(bins = 30, fill = 'blue', alpha = 0.7) +
    theme_minimal() +
    ggtitle('Distribución de Temperaturas en Celsius')
  print(p)
}

# Función para visualizar datos finales
final_data <- function(df) {
  print("################## DATA FINAL ##################")
  print(summary(df))
  #View(df)
}

main <- function(file_path) {
  
  initial_df <- load_data(file_path)
  exploration_data(initial_df)
  plot_temperature_distribution_Fahrenheit(initial_df)
  cleaned_df <- clean_data(initial_df)
  plot_temperature_distribution_Celsius(cleaned_df)
  final_data(cleaned_df)
  
  
}

main("../data/city_temperature.csv")
