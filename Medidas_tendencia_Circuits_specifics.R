# Cargar las librerías necesarias
library(readr)
library(ggplot2)

# Especificar la ruta correcta del archivo
ruta_archivo <- "C:/Users/rjime/Desktop/TFG- BA/Anteproyecto/Fuentes de las BDDD/Propias/Circuits_specifics_corrected.csv"

# Leer el archivo con manejo de codificación y separador
Circuits_specifics <- read.csv(ruta_archivo, sep=",", header=TRUE, stringsAsFactors=FALSE, fileEncoding="UTF-8")

# Verificar la estructura del archivo
print(str(Circuits_specifics))

# Ver nombres de columnas
print(names(Circuits_specifics))

# Convertir TODAS las columnas numéricas correctamente
cols_to_numeric <- c("Nº.C3", "Nº.C2", "NºC1", "NºCI", "NºCW")

# Filtrar columnas que realmente existen en el DataFrame
cols_to_convert <- intersect(cols_to_numeric, names(Circuits_specifics))

# Aplicar conversión solo a las columnas encontradas
for (col in cols_to_convert) {
  Circuits_specifics[[col]] <- as.numeric(gsub(",", ".", Circuits_specifics[[col]])) 
}

# Seleccionar solo las variables numéricas después de la conversión
numeric_vars <- Circuits_specifics[, sapply(Circuits_specifics, is.numeric)]

# Calcular medidas estadísticas
mean_values <- apply(numeric_vars, 2, mean, na.rm = TRUE)
sd_values <- apply(numeric_vars, 2, sd, na.rm = TRUE)
var_values <- apply(numeric_vars, 2, var, na.rm = TRUE)

# Crear un data frame con los resultados
stats_table <- data.frame(
  Variable = names(mean_values),
  Media = mean_values,
  Desviación_Estandar = sd_values,
  Varianza = var_values
)

# Mostrar la tabla en consola
print(stats_table)

# Visualizar las medidas estadísticas con gráficos separados para cada métrica
ggplot(stats_table, aes(x = Variable)) +
  geom_bar(aes(y = Media, fill = "Media"), stat = "identity", position = "dodge", color="black") +
  geom_bar(aes(y = Desviación_Estandar, fill = "Desviación Estándar"), stat = "identity", position = "dodge", color="black") +
  geom_bar(aes(y = Varianza, fill = "Varianza"), stat = "identity", position = "dodge", color="black") +
  labs(title = "Medidas Estadísticas por Variable", x = "Variable", y = "Valor") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(name = "Medidas", values = c("Media" = "blue", "Desviación Estándar" = "green", "Varianza" = "red")) +
  theme(plot.title = element_text(hjust = 0.5)) # Centrar título del gráfico
