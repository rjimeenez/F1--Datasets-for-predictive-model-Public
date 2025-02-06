# Cargar librerías necesarias
library(readr)
library(dplyr)
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

# Lista de equipos de la temporada 2024
equipos_2024 <- c("McLaren-Mercedes", "Ferrari", "Red Bull Racing-Honda RBPT", "Mercedes",
                  "Aston Martin-Mercedes", "Alpine-Renault", "Williams-Mercedes", 
                  "Alfa Romeo-Ferrari", "Haas-Ferrari", "AlphaTauri-RBPT")

# Datos de victorias acumuladas con nombres históricos corregidos
equipos_historicos <- data.frame(
  Equipo_Actual = c("McLaren-Mercedes", "Ferrari", "Red Bull Racing-Honda RBPT", "Mercedes",
                    "Aston Martin-Mercedes", "Alpine-Renault", "Williams-Mercedes", 
                    "Alfa Romeo-Ferrari", "Haas-Ferrari", "AlphaTauri-RBPT"),
  Total_Victorias = c(189, 248, 123, 163, 5, 144, 117, 11, 0, 2) # Datos corregidos
)

# Filtrar datos históricos, asegurando que no se excluyan equipos con pocos registros**
wdc_filtrado_equipos <- wdc_data %>%
  filter(Equipo %in% equipos_2024) %>%
  filter(Año >= 2000)  # Asegurar que el rango de datos sea desde el año 2000

# Verificar la cantidad de registros por equipo para asegurar líneas en la gráfica
conteo_por_equipo <- wdc_filtrado_equipos %>%
  group_by(Equipo) %>%
  summarise(Conteo = n(), .groups = "drop")

# Graficar solo los equipos con suficiente información
equipos_a_graficar <- conteo_por_equipo$Equipo[conteo_por_equipo$Conteo > 2]

# Filtrar datos solo para equipos con suficientes datos
wdc_filtrado_equipos <- wdc_filtrado_equipos %>%
  filter(Equipo %in% equipos_a_graficar)

# 1 Gráfico de dispersión con líneas de tendencia aseguradas**
ggplot(wdc_filtrado_equipos, aes(x = Año, y = Puntos_Ajustados, color = Equipo, group = Equipo)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  labs(title = "Relación entre Años y Puntos Acumulados por Equipo",
       x = "Año",
       y = "Puntos Acumulados",
       color = "Equipo") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 2 Histograma de victorias acumuladas con etiquetas**
ggplot(equipos_historicos, aes(x = Total_Victorias, y = reorder(Equipo_Actual, Total_Victorias), fill = factor(Equipo_Actual))) +
  geom_bar(stat = "identity", color = "black", alpha = 0.7) +
  geom_text(aes(label = Total_Victorias), hjust = -0.3, size = 5, fontface = "bold") +  
  labs(title = "Distribución del Número de Victorias por Equipo (2024)",
       subtitle = "Solo equipos de la temporada 2024 con victorias históricas",
       x = "Número de Victorias",
       y = "Equipo",
       fill = "Equipo") +
  scale_x_continuous(breaks = seq(0, max(equipos_historicos$Total_Victorias, na.rm = TRUE), by = 10), 
                     limits = c(0, max(equipos_historicos$Total_Victorias, na.rm = TRUE) + 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3 Gráfico de líneas: Evolución temporal de puntos acumulados por equipo**
ggplot(wdc_filtrado_equipos, aes(x = Año, y = Puntos_Ajustados, group = Equipo, color = Equipo)) +
  geom_line(linewidth = 1) +  
  geom_point(size = 2) +
  labs(title = "Evolución Temporal de los Puntos Acumulados por Equipo (2000-2024)",
       x = "Año",
       y = "Puntos Acumulados",
       color = "Equipo") +  
  theme_minimal() +
  theme(legend.position = "bottom")
