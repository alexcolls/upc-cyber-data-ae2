# Install required libraries if not already installed
if (!require("httr")) install.packages("httr")
if (!require("XML")) install.packages("XML")

# Load libraries
library(httr)
library(XML)

# Define the URL
url <- "https://www.mediawiki.org/wiki/MediaWiki"

# Define the file to save the HTML content
output_file <- "mediawiki_page.html"

# Download the webpage
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Save the HTML content to a file
  html_content <- content(response, as = "text", encoding = "UTF-8")
  writeLines(html_content, output_file)
  cat("Webpage successfully downloaded and saved to:", output_file, "\n")
  # Parse the saved HTML file
  parsed_html <- htmlParse(file = output_file, encoding = "UTF-8")
  cat("HTML file successfully parsed.\n")
} else {
  stop("Failed to download the webpage. HTTP status code:", status_code(response))
}
