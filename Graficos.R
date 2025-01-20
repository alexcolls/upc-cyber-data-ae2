# Cargar las librerías necesarias
library(httr)
library(XML)
library(ggplot2)
library(dplyr)

# Función principal mejorada con ajustes de tamaño y encabezado
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
  
  # Extraer enlaces
  enlaces_url <- xpathSApply(xml_parsed, "//a/@href")
  enlaces_url <- unlist(enlaces_url)
  
  # Crear un data frame inicial
  enlaces_df <- data.frame(
    URL = enlaces_url,
    stringsAsFactors = FALSE
  )
  
  # Clasificar URLs como absolutas o relativas
  enlaces_df <- enlaces_df %>%
    mutate(
      Tipo = ifelse(grepl("^http", URL), "Absoluta", "Relativa")
    )
  
  # Contar frecuencias de aparición de las URLs por tipo
  enlaces_recuento <- enlaces_df %>%
    group_by(Tipo) %>%
    summarise(Frecuencia = n())
  
  # Crear histograma con diseño profesional y ajustes de tamaño
  histograma <- ggplot(enlaces_recuento, aes(x = Tipo, y = Frecuencia, fill = Tipo)) +
    geom_bar(stat = "identity", color = "black", width = 0.5) +
    scale_fill_manual(values = c("Absoluta" = "#4B9CD3", "Relativa" = "#9ACD32")) +
    labs(
      title = "Frecuencia de URLs por Tipo",
      subtitle = "Comparación entre URLs absolutas y relativas",
      x = "Tipo de URL",
      y = "Frecuencia",
      fill = "Tipo de URL"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 10),
      axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
      legend.position = "top",
      legend.title = element_text(size = 10, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10) # Reducir márgenes
    ) +
    geom_text(
      aes(label = Frecuencia), 
      vjust = -0.5, 
      size = 4, 
      color = "black",
      fontface = "bold"
    ) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) # Separar el título de la leyenda
  
  # Guardar el histograma en un archivo PDF más pequeño
  ggsave("Histograma_Frecuencia_URLs_Pequeño.pdf", histograma, width = 6, height = 4) # Tamaño reducido
  
  # Mostrar el histograma en la consola
  print(histograma)
  
  # Retornar resultados
  return(list(
    TablaFrecuencias = enlaces_recuento
  ))
}

# Ejecutar la función con una URL de ejemplo
resultado <- procesar_pagina_histograma("https://www.mediawiki.org/wiki/MediaWiki")



