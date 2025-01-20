# Cargar las librerías necesarias
library(httr)
library(XML)
library(ggplot2)
library(dplyr)

# Función principal
procesar_pagina_histograma <- function(url) {
  # Descargar el contenido de la página
  response <- GET(url)
  
  if (status_code(response) != 200) {
    cat("Error al descargar la página. Código de estado:", status_code(response), "\n")
    return(NULL)
  }
  
  # Extraer y parsear el contenido HTML
  html_content <- content(response, as = "text", encoding = "UTF-8")
  xml_parsed <- htmlParse(html_content, encoding = "UTF-8")
  
  # 1. Extraer enlaces y textos
  enlaces_url <- xpathSApply(xml_parsed, "//a/@href")
  enlaces_url <- unlist(enlaces_url)
  
  # Crear un data frame inicial
  enlaces_df <- data.frame(
    URL = enlaces_url,
    stringsAsFactors = FALSE
  )
  
  # Categorizar URLs como absolutas o relativas
  enlaces_df <- enlaces_df %>%
    mutate(
      Tipo = ifelse(grepl("^http", URL), "Absoluta", "Relativa")
    )
  
  # Contar frecuencias de aparición de las URLs por tipo
  enlaces_recuento <- enlaces_df %>%
    group_by(Tipo) %>%
    summarise(Frecuencia = n())
  
  # Crear histograma con ggplot2
  histograma <- ggplot(enlaces_recuento, aes(x = Tipo, y = Frecuencia, fill = Tipo)) +
    geom_bar(stat = "identity", color = "black", width = 0.7) +
    scale_fill_manual(values = c("Absoluta" = "#1f78b4", "Relativa" = "#33a02c")) +
    labs(
      title = "Frecuencia de URLs por Tipo",
      x = "Tipo de URL",
      y = "Frecuencia",
      fill = "Tipo de URL"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "none"
    )
  
  # Guardar el histograma en un archivo PDF
  ggsave("Histograma_Frecuencia_URLs.pdf", histograma, width = 8, height = 5)
  
  # Mostrar el histograma en la consola
  print(histograma)
  
  # Retornar resultados
  return(list(
    TablaFrecuencias = enlaces_recuento
  ))
}

# Ejecutar la función con una URL de ejemplo
resultado <- procesar_pagina_histograma("https://www.mediawiki.org/wiki/MediaWiki")

