# Cargar librerías necesarias
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Especificar la ruta del archivo
ruta_archivo <- "C:/Users/rjime/Desktop/TFG- BA/Anteproyecto/Fuentes de las BDDD/Propias/WDC_Drivers_Constructors_corrected.csv"

# Leer el archivo con codificación adecuada
wdc_data <- read.csv(ruta_archivo, sep = ",", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")

# Verificar estructura antes de renombrar columnas
if (ncol(wdc_data) == 1) {
  wdc_data <- read.csv(ruta_archivo, sep = ";", header = TRUE, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
}

# Renombrar columnas si es necesario
if (ncol(wdc_data) == 9) {
  colnames(wdc_data) <- c("Año", "Categoría", "Posición", "Nombre", "Nacionalidad", "Equipo", 
                          "Puntos_Originales", "Puntos_Ajustados", "Sistema_Puntuación")
} else {
  stop("Error: El número de columnas en el archivo no coincide con el esperado. Revise el CSV.")
}

# Convertir las columnas de interés a valores numéricos
wdc_data$Posición <- as.numeric(gsub("[^0-9.]", "", wdc_data$Posición))
wdc_data$Puntos_Ajustados <- as.numeric(gsub("[^0-9.]", "", wdc_data$Puntos_Ajustados))
wdc_data$Victorias <- as.numeric(gsub("[^0-9.]", "", wdc_data$Puntos_Originales)) 
wdc_data$Año <- as.numeric(gsub("[^0-9.]", "", wdc_data$Año))

# Listado de pilotos de la temporada 2024
pilotos_2024 <- c("Max Verstappen", "Lando Norris", "Charles Leclerc", "Lewis Hamilton", "Carlos Sainz Jr.",
                  "George Russell", "Sergio Pérez", "Fernando Alonso", "Oscar Piastri", "Pierre Gasly",
                  "Esteban Ocon", "Lance Stroll", "Alexander Albon", "Valtteri Bottas", "Yuki Tsunoda",
                  "Kevin Magnussen", "Guanyu Zhou", "Nico Hülkenberg", "Logan Sargeant", "Liam Lawson",
                  "Daniel Ricciardo", "Nyck de Vries")

# Filtrar datos históricos, pero solo para pilotos que están en la temporada 2024
wdc_filtrado <- wdc_data %>%
  filter(Nombre %in% pilotos_2024) %>%
  filter(Año >= 2000)  # Asegurar que el rango de datos sea desde el año 2000

# Datos reales de victorias históricas para la parrilla 2024
victorias_correctas <- data.frame(
  Nombre = c("Lewis Hamilton", "Max Verstappen", "Fernando Alonso", "Valtteri Bottas", "Pierre Gasly",
             "Charles Leclerc", "Lando Norris", "Esteban Ocon", "Sergio Pérez", "Oscar Piastri",
             "Daniel Ricciardo", "George Russell", "Carlos Sainz Jr."),
  Total_Victorias = c(105, 63, 32, 10, 1, 8, 4, 1, 6, 2, 8, 3, 4) # Datos corregidos
)

# Ordenar los pilotos por victorias de mayor a menor
victorias_correctas <- victorias_correctas %>% arrange(desc(Total_Victorias))

# 1. Diagrama de dispersión: Año vs. Puntos Acumulados
ggplot(wdc_filtrado, aes(x = Año, y = Puntos_Ajustados, color = Nombre)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Relación entre Años y Puntos Acumulados",
       x = "Año",
       y = "Puntos Acumulados",
       color = "Piloto") +
  theme_minimal()

# 2. Histograma mejorado con etiquetas de pilotos
ggplot(victorias_correctas, aes(x = Total_Victorias, y = reorder(Nombre, Total_Victorias), fill = factor(Nombre))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = Total_Victorias), hjust = -0.3, size = 5, fontface = "bold") +  
  labs(title = "Distribución del Número de Victorias por Piloto (2024)",
       subtitle = "Solo pilotos de la temporada 2024 con victorias históricas",
       x = "Número de Victorias",
       y = "Piloto",
       fill = "Piloto") +
  scale_x_continuous(breaks = seq(0, max(victorias_correctas$Total_Victorias, na.rm = TRUE), by = 10), 
                     limits = c(0, max(victorias_correctas$Total_Victorias, na.rm = TRUE) + 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. Gráfico de líneas: Evolución temporal de puntos acumulados por piloto
ggplot(wdc_filtrado, aes(x = Año, y = Puntos_Ajustados, group = Nombre, color = Nombre)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) +
  labs(title = "Evolución Temporal de los Puntos Acumulados por Piloto (2000-2024)",
       x = "Año",
       y = "Puntos Acumulados",
       color = "Piloto") +  
  theme_minimal() +
  theme(legend.position = "bottom")
