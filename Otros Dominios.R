# Instalar dependencias
if (!require("ggplot2")) install.packages("ggplot2")

# Cargar librerías
library(ggplot2)

# Cargar los datos desde el archivo
file_path <- "./tabla_enlaces_estado.csv"
enlaces <- read.csv(file_path, stringsAsFactors = FALSE)

# Asegurarse de que las columnas tengan el formato correcto
enlaces$Frecuencia <- as.numeric(enlaces$Frecuencia)

# Convertir NULLs o valores vacíos a NA
enlaces[enlaces == ""] <- NA
enlaces <- na.omit(enlaces)

# Filtrar enlaces internos y externos
url_base <- "https://www.mediawiki.org"
enlaces$Tipo <- ifelse(grepl(paste0("^", url_base), enlaces$URL), "Interno", "Externo")

# Sumar las frecuencias por tipo de enlace
resumen <- aggregate(Frecuencia ~ Tipo, data = enlaces, sum, na.rm = TRUE)

# Crear el gráfico
grafico <- ggplot(resumen, aes(x = Tipo, y = Frecuencia, fill = Tipo)) +
  geom_bar(stat = "identity") +
  labs(title = "Comparación de enlaces internos y externos",
       x = "Tipo de enlace",
       y = "Suma de frecuencias") +
  theme_minimal()

# Exportar el gráfico a un archivo PDF
pdf("grafico_otros_dominios.pdf", width = 8, height = 6)
print(grafico)
dev.off()

# Confirmación
cat("El gráfico ha sido exportado a 'grafico_enlaces.pdf'\n")
