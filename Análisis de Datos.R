# Cargar las librerías necesarias
library(httr)
library(XML)

# Función principal
procesar_pagina <- function(url) {
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
        NA  # Ignorar tags internos como "#"
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
      Sys.sleep(1)  # Esperar 1 segundo entre peticiones
      head_response <- HEAD(link)
      head_response$status_code
    }, error = function(e) {
      return(NA)
    })
  })
  
  # Crear y retornar un data frame con toda la información
  return(data.frame(
    URL = tabla_enlaces$URL,
    Texto = tabla_enlaces$Texto,
    Frecuencia = tabla_enlaces$Frecuencia,
    Estado = tabla_enlaces$Estado,
    stringsAsFactors = FALSE
  ))
}

# Ejecutar la función con una URL de ejemplo
resultado <- procesar_pagina("https://www.mediawiki.org/wiki/MediaWiki")

# Mostrar el resultado como data frame
print(resultado)

# Guardar el resultado en un archivo CSV (opcional)
write.csv(resultado, "tabla_enlaces_estado.csv", row.names = FALSE)
