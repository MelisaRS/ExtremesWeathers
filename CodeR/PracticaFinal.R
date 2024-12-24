# Cargar librerías
library(ggplot2)

# Establecer el directorio de trabajo al directorio del archivo .R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Función para cargar y visualizacion de los datos
load_data <- function(file_path) {
  df <- read.csv(file_path) 
  View(data)
  print(head(df))
  # Mostrar el resumen de los datos
  summary_statistics(df)
  return(df)
}

exploration_data <- function(df){
  
  # Country
  #print(unique(df$Country)) 
  num_countries <- length(unique(df$Country))
  cat("El número de ciudades únicas es:", num_countries, "\n")
  
  countries_states(df)
  
  # Llamar a la función con tu dataset
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
  cat("Países encontrados en el dataset:\n")
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
  
  # Imprimir el número total de estados únicos
  cat("Número total de estados únicos:", total_states, "\n")
  
  # Imprimir la lista de países correspondientes
  cat("Países correspondientes:\n")
  print(unique_countries)
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
  print("Resultados de la limpieza de NA")
  cat("Total de filas antes de la limpieza:", total_filas_anterior, "\n")
  cat("Total de filas después de la limpieza:", total_filas_despues, "\n")
  cat("Cantidad de NA eliminados:", total_filas_anterior - total_filas_despues, "\n")
  cat("Total de NAs en el dataset antes de la limpieza:", na_count_before, "\n")
  cat("Total de NAs en el dataset después de la limpieza:", na_count_after, "\n")
  
  #eliminando columna State
  df <- df[, !names(df) %in% c("State")]
  print("Columna State eliminada")
  
  # Crear una nueva columna Date combinando Year, Month y Day
  df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"), format = "%Y-%m-%d")
  
  print(head(df))
  
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
  
  exploration_data(df)

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
