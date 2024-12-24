# Cargar librerías
library(ggplot2)

# Establecer el directorio de trabajo al directorio del archivo .R
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Función para cargar y visualizacion de los datos
load_data <- function(file_path) {
  df <- read.csv(file_path) 
  View(data)
  print("################## DATA INICIAL ##################")
  print(head(df))
  print("################## SUMMARY INICIAL ##################")
  # Mostrar el resumen de los datos
  summary_statistics(df)
  return(df)
}

exploration_data <- function(df){
  
  print("################## EXPLORACION DE DATOS ##################")
  # Country
  #print(unique(df$Country)) 
  num_countries <- length(unique(df$Country))
  cat("El número de ciudades es:", num_countries, "\n")
  
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
  
  # Imprimir el número total de estados únicos
  cat("Número total de estados:", total_states, "\n")
  
  # Imprimir la lista de países correspondientes
  cat("Países correspondientes de todos los estados:\n")
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
  
  # Crear una nueva columna Date combinando Year, Month y Day
  df$Date <- as.Date(paste(df$Year, df$Month, df$Day, sep = "-"), format = "%Y-%m-%d")
  
  #eliminando columna Innecesarias
  df <- df[, !names(df) %in% c("State", "Day", "Month", "Year")]
  print("Columna State, Day, Month y Year eliminadas")
  
  print("################## NUEVO DATASET ##################")
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

# Función para visualizar datos finales
final_data <- function(df) {
  print("################## DATA FINAL ##################")
  # Mostrar el resumen de los datos
  print(summary(df))
  View(df)
}

# Función para crear un boxplot
plot_boxplot <- function(df, title) {
  p <-  ggplot(df, aes(y = AvgTemperature)) +
        geom_boxplot(fill = "blue", alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
        theme_minimal() +
        labs(title = title, y = "AvgTemperature (Fahrenheit)") +
        theme(plot.title = element_text(hjust = 0.5))
  # Devolver el objeto de la gráfica para que pueda ser visualizado
  print(p)
}

plot_boxplot2 <- function(df, title){
  p <- boxplot(df$AvgTemperature,
               main = title,
               ylab = "avgTemperature (Fahrenheit)",
               col = "lightblue",
               outline = TRUE)
  # Devolver el objeto de la gráfica para que pueda ser visualizado
  return(p)
}

plot_temperature_history <- function(df, country) {
  # Filtrar el dataset para el país seleccionado
  df_us <- subset(df, Country == country)
  
  # Crear el gráfico
  p <- ggplot(df_us, aes(x = Date, y = AvgTemperature_Celsius)) +
    geom_line(color = "blue") +
    labs(title = paste("Historial de Temperaturas en", country),
         x = "Fecha",
         y = "Temperatura promedio (°C)") +
    theme_minimal()
  
  # Mostrar el gráfico
  print(p)
}

save_cleaned_df <- function(df, file_path){
  # Guardar el dataset limpio en un archivo CSV
  write.csv(df, file_path, row.names = FALSE)
}


# Función principal para ejecutar todo el proceso
main <- function(file_path) {
  
  # Cargar los datos
  initial_df <- load_data(file_path)
  
  exploration_data(initial_df)

  # Mostrar la distribución de temperaturas en Fahrenheit
  plot_temperature_distribution_Fahrenheit(initial_df)
  
  # Crear boxplot para el dataset inicial
  plot_initial <- plot_boxplot(initial_df, "Boxplot de AvgTemperature (Dataset Inicial)")
  #print(plot_initial)
  
  # Limpiar los datos
  cleaned_df <- clean_data(initial_df)
  
  # Convertir a Celsius
  cleaned_df <- convert_to_celsius(cleaned_df)
  
  # Verificar las primeras filas de la conversión
  print(head(cleaned_df[, c("AvgTemperature", "AvgTemperature_Celsius")]))
  
  # Mostrar la distribución de temperaturas en Celsius
  plot_temperature_distribution_Celsius(cleaned_df)
  
  # Crear boxplot para el dataset limpio
  plot_cleaned <- plot_boxplot(cleaned_df, "Boxplot de AvgTemperature (Dataset Limpio)")
  #print(plot_cleaned)
  
  final_data(cleaned_df)
  
  #print(colnames(cleaned_df))
  #print(unique(cleaned_df$Country))
  
  # Llamar a la función para graficar las temperaturas de US
  plot_temperature_history(cleaned_df, "US")
  #plot_temperature_history(cleaned_df, "Bolivia")
  
  save_cleaned_df(cleaned_df, "../data/cleaned_city_temperature.csv")
  
}

# Ejecutar el script (cambiar la ruta del archivo según corresponda)
main("../data/city_temperature.csv")
