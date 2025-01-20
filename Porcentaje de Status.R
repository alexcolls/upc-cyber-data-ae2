# Instalar librerías necesarias
if (!require("httr")) install.packages("httr")
if (!require("XML")) install.packages("XML")
if (!require("ggplot2")) install.packages("ggplot2")

# Cargar librerías
library(httr)
library(XML)
library(ggplot2)

# URL base y completa
base_url <- "https://www.mediawiki.org"
page_url <- "https://www.mediawiki.org/wiki/MediaWiki"

# 1. Descargar y parsear la página
response <- GET(page_url)
parsed_html <- htmlParse(content(response, "text"))

# 2. Extraer enlaces (etiqueta <a>)
links <- xpathSApply(parsed_html, "//a", function(x) xmlGetAttr(x, "href"))

# Limpiar enlaces (quitar valores NULL y NA)
links <- links[!is.null(links)]
links <- na.omit(links)

# Convertir enlaces relativos a absolutos
links <- ifelse(grepl("^http", links), links, paste0(base_url, links))
links <- unique(links) # Eliminar duplicados

# 3. Analizar enlaces: obtener códigos de estado HTTP
get_status <- function(link) {
  tryCatch({
    res <- HEAD(link)
    return(res$status_code)
  }, error = function(e) {
    return(NA) # En caso de error, devolver NA
  })
}

# Aplicar la función a todos los enlaces
status_codes <- sapply(links, get_status)

# Crear un data.frame con los resultados
status_data <- data.frame(
  Link = links,
  Status_Code = status_codes
)

# 4. Agrupar por código de estado y calcular porcentajes
status_summary <- as.data.frame(table(status_data$Status_Code))
colnames(status_summary) <- c("Status", "Frequency")
status_summary$Percentage <- (status_summary$Frequency / sum(status_summary$Frequency)) * 100

# 5. Crear gráfico de tarta
pie_chart <- ggplot(status_summary, aes(x = "", y = Percentage, fill = Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribución de códigos de estado HTTP") +
  theme_void() +
  theme(legend.title = element_text(size = 10), 
        legend.text = element_text(size = 8))

# Mostrar el gráfico
print(pie_chart)

# Guardar resultados
write.csv(status_data, "status_data.csv", row.names = FALSE)
write.csv(status_summary, "status_summary.csv", row.names = FALSE)
