# Cargar las librerías necesarias
library(httr)
library(XML)
library(ggplot2)
library(dplyr)

# Función principal
procesar_pagina_con_grafico <- function(url) {
  # Descargar el contenido de la página
  response <- GET(url)
  
  if (status_code(response) != 200) {
    cat("Error al descargar la página. Código de estado:", status_code(response), "\n")
    return(NULL)
  }
  
  # Extraer y parsear el contenido HTML
  html_content <- content(response, as = "text", encoding = "UTF-8")
  xml_parsed <- htmlParse(html_content, encoding = "UTF-8")
  
  # 1. Extraer la cabecera (título)
  cabecera <- xpathSApply(xml_parsed, "//title", xmlValue)
  cat("Cabecera:", cabecera, "\n\n")
  
  # 2. Extraer enlaces y textos
  enlaces_texto <- xpathSApply(xml_parsed, "//a", xmlValue)
  enlaces_url <- xpathSApply(xml_parsed, "//a/@href")
  
  # Convertir a vectores simples y manejar nulos
  enlaces_texto[is.null(enlaces_texto)] <- NA
  enlaces_url[is.null(enlaces_url)] <- NA
  enlaces_texto <- unlist(enlaces_texto)
  enlaces_url <- unlist(enlaces_url)
  
  # Crear un data frame inicial
  enlaces_df <- data.frame(
    URL = enlaces_url,
    Texto = enlaces_texto,
    stringsAsFactors = FALSE
  )
  
  # Ajustar URLs relativas y absolutas
  enlaces_df$URL <- ifelse(
    grepl("^http", enlaces_df$URL), 
    enlaces_df$URL,
    ifelse(
      grepl("^//", enlaces_df$URL), 
      paste0("https:", enlaces_df$URL),
      ifelse(
        grepl("^/", enlaces_df$URL), 
        paste0(url, enlaces_df$URL),
        NA
      )
    )
  )
  
  # Contar la frecuencia de cada enlace
  enlaces_recuento <- as.data.frame(table(enlaces_df$URL))
  colnames(enlaces_recuento) <- c("URL", "Frecuencia")
  enlaces_unicos <- enlaces_df[!duplicated(enlaces_df$URL), ]
  tabla_enlaces <- merge(enlaces_unicos, enlaces_recuento, by = "URL", all.x = TRUE)
  
  # Verificar el estado HTTP de cada enlace
  tabla_enlaces$Estado <- sapply(tabla_enlaces$URL, function(link) {
    if (is.na(link)) return(NA)
    tryCatch({
      Sys.sleep(1)
      head_response <- HEAD(link)
      head_response$status_code
    }, error = function(e) {
      return(NA)
    })
  })
  
  # Filtrar para mostrar los 15 enlaces más frecuentes con frecuencia hasta 6
  enlaces_top <- tabla_enlaces %>% 
    arrange(desc(Frecuencia)) %>% 
    slice_head(n = 15) %>% 
    filter(Frecuencia <= 6)
  
  # Crear gráfico usando ggplot2 con geom_count
  grafico <- ggplot(enlaces_top, aes(x = reorder(Texto, -Frecuencia), y = Frecuencia, size = Frecuencia, fill = as.factor(Frecuencia))) +
    geom_point(shape = 21, color = "black") +
    scale_size_continuous(range = c(3, 10)) +
    scale_fill_brewer(palette = "Set2") +
    labs(
      title = "Top 15 Enlaces Más Frecuentes",
      x = "Texto del Enlace",
      y = "Frecuencia",
      size = "Frecuencia",
      fill = "Frecuencia"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
  
  # Guardar gráfico en PDF
  ggsave("Enlaces_mas_frecuentes.pdf", grafico, width = 10, height = 6)
  
  # Mostrar gráfico en la consola
  print(grafico)
  
  # Guardar datos en un archivo CSV
  write.csv(enlaces_top, "tabla_enlaces_Mas_Frecuentes.csv", row.names = FALSE)
  
  # Retornar resultados
  return(list(
    Cabecera = cabecera,
    TablaEnlaces = enlaces_top
  ))
}

# Ejecutar la función con una URL de ejemplo
resultado <- procesar_pagina_con_grafico("https://www.mediawiki.org/wiki/MediaWiki")
