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

# Renombrar columnas
if (ncol(wdc_data) == 9) {
  colnames(wdc_data) <- c("Año", "Categoría", "Posición", "Nombre", "Nacionalidad", "Equipo", 
                          "Puntos_Originales", "Puntos_Ajustados", "Sistema_Puntuación")
} else {
  stop("Error: El número de columnas en el archivo no coincide con el esperado. Revise el CSV.")
}

# Convertir las columnas de interés a valores numéricos
wdc_data$Posición <- as.numeric(gsub("[^0-9.]", "", wdc_data$Posición))
wdc_data$Puntos_Ajustados <- as.numeric(gsub("[^0-9.]", "", wdc_data$Puntos_Ajustados))

# Listado de pilotos y equipos de interés (solo los de la temporada 2024)
pilotos_2024 <- c("Max Verstappen", "Lando Norris", "Charles Leclerc", "Lewis Hamilton", "Carlos Sainz Jr.",
                  "George Russell", "Sergio Pérez", "Fernando Alonso", "Oscar Piastri", "Pierre Gasly",
                  "Esteban Ocon", "Lance Stroll", "Alexander Albon", "Valtteri Bottas", "Yuki Tsunoda",
                  "Kevin Magnussen", "Guanyu Zhou", "Nico Hülkenberg", "Logan Sargeant", "Liam Lawson",
                  "Daniel Ricciardo", "Nyck de Vries")

equipos_2024 <- c("McLaren-Mercedes", "Ferrari", "Red Bull Racing-Honda RBPT", "Mercedes",
                  "Aston Martin-Mercedes", "Alpine-Renault", "Williams-Mercedes", "Alfa Romeo-Ferrari",
                  "Haas-Ferrari", "AlphaTauri-RBPT")

# Filtrar datos históricos, pero solo para pilotos y equipos que están en la temporada 2024
wdc_filtrado <- wdc_data %>%
  filter(Nombre %in% pilotos_2024 | Equipo %in% equipos_2024) %>%
  filter(!(is.na(Nombre) & is.na(Equipo)))  # Asegurarse de eliminar registros sin piloto ni equipo

# Calcular estadísticas históricas por piloto
estadisticas_pilotos <- wdc_filtrado %>%
  filter(Nombre %in% pilotos_2024) %>%  # Solo pilotos en la temporada 2024
  group_by(Nombre) %>%
  summarise(
    Total_Años = n_distinct(Año),
    Posicion_Media = mean(Posición, na.rm = TRUE),
    Posicion_Mediana = median(Posición, na.rm = TRUE),
    Posicion_Moda = ifelse(length(table(Posición)) > 0, 
                           as.numeric(names(sort(table(Posición), decreasing = TRUE)[1])), NA),
    Posicion_SD = sd(Posición, na.rm = TRUE),
    Puntos_Medios = mean(Puntos_Ajustados, na.rm = TRUE),
    Puntos_Mediana = median(Puntos_Ajustados, na.rm = TRUE),
    Puntos_Moda = ifelse(length(table(Puntos_Ajustados)) > 0, 
                         as.numeric(names(sort(table(Puntos_Ajustados), decreasing = TRUE)[1])), NA),
    Puntos_SD = sd(Puntos_Ajustados, na.rm = TRUE)
  )

# Calcular estadísticas históricas por equipo
estadisticas_equipos <- wdc_filtrado %>%
  filter(Equipo %in% equipos_2024) %>%  # Solo equipos en la temporada 2024
  group_by(Equipo) %>%
  summarise(
    Total_Años = n_distinct(Año),
    Posicion_Media = mean(Posición, na.rm = TRUE),
    Posicion_Mediana = median(Posición, na.rm = TRUE),
    Posicion_Moda = ifelse(length(table(Posición)) > 0, 
                           as.numeric(names(sort(table(Posición), decreasing = TRUE)[1])), NA),
    Posicion_SD = sd(Posición, na.rm = TRUE),
    Puntos_Medios = mean(Puntos_Ajustados, na.rm = TRUE),
    Puntos_Mediana = median(Puntos_Ajustados, na.rm = TRUE),
    Puntos_Moda = ifelse(length(table(Puntos_Ajustados)) > 0, 
                         as.numeric(names(sort(table(Puntos_Ajustados), decreasing = TRUE)[1])), NA),
    Puntos_SD = sd(Puntos_Ajustados, na.rm = TRUE)
  )

# Validar que las tablas de estadísticas contengan únicamente los pilotos y equipos deseados
print("Estadísticas por Pilotos (filtradas):")
print(estadisticas_pilotos)

print("Estadísticas por Equipos (filtradas):")
print(estadisticas_equipos)

# Crear gráficos para pilotos
estadisticas_pilotos_long <- estadisticas_pilotos %>%
  pivot_longer(cols = -Nombre, names_to = "Métrica", values_to = "Valor") %>%
  filter(!is.na(Valor)) # Eliminar valores faltantes

ggplot(estadisticas_pilotos_long, aes(x = Nombre, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Estadísticas Históricas por Pilotos (2024)", x = "Piloto", y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Crear gráficos para equipos
estadisticas_equipos_long <- estadisticas_equipos %>%
  pivot_longer(cols = -Equipo, names_to = "Métrica", values_to = "Valor") %>%
  filter(!is.na(Valor)) # Eliminar valores faltantes

ggplot(estadisticas_equipos_long, aes(x = Equipo, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Estadísticas Históricas por Equipos (2024)", x = "Equipo", y = "Valor") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")
