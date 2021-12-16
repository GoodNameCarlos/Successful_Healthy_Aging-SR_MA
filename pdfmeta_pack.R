################################################################################

# Título: pdf_meta
# Proyecto: Factores asociados con el desarrollo de envejecimiento exitoso y 
#           saludable: Revisión Sistemática y metaanálisis de estudios 
#           longitudinales.
# Autor: Carlos González-Carballo 
# email: carlos.gz.cb@gmail.com 
# Script: Todos los paquetes y funciones utilizadas en el trabajo de meta analisis. 
# Data: datos de PDF para metaanálisis. 
# Copyright: Este Script es producto de Carlos González-Carballo como trabajo de
#            investigación del Doctorado en ciencias, Epidemiología.

#                              STATUS: Stable

################################################################################

# Paquetes
library(tidyverse)
library(here)
# library(pdftools)
library(purrr)


# funciones creadas para la extracción de datos ---------------------------

## Genera tabla a partir de objeto string
pdf_tables <- function(data) {
  # requiere de tidiverse y stringr
  require(tidyverse)
  if (is.character(data) == TRUE) {
    data %>% 
      str_trim() %>% 
      str_split("\\s{2,}", simplify = T) %>% 
      data.frame(stringsAsFactors = F) %>%  
      as_tibble()
  } else {
    warning("No character object")
  }
}

## Extracción de tablas
tables_extract <- function(.data, variables, orvar, icvar, separator, dbl = FALSE) {
  # La opción dbl es una opción lógica, donde el valor verdadero indica que 
  # contiene un signo de puntuación en la variable de intervalo de confianza 
  # que se va a eliminar. El valor ppredeterminado FALSE no requiere que se 
  # elimine un signo de púntuación. 
  require(tidyverse)
  require(stringr)
  
  if (dbl == TRUE & is_tibble(.data) == TRUE) { # contiene un signo de puntuación específico
    
    .data %>% 
      select({{ variables }}) %>% 
      mutate(tmp_chunks = str_split({{ icvar }}, pattern = {{ separator }}, n = 2)) %>%   
      mutate(lower = map_chr(tmp_chunks, 1), 
             upper = map_chr(tmp_chunks, 2)) %>% 
      select(-tmp_chunks, -{{ icvar }}) %>% 
      rename(characteristics = X1, 
             OR = {{orvar}}) %>% 
      mutate(OR = str_extract(string = OR, pattern = "\\d{1,2}\\.\\d{1,2}|\\.\\d{1,2}|\\d"), 
             lower = str_extract(string = lower, pattern = "\\d{1,2}\\.\\d{1,2}|\\.\\d{1,2}|\\d"), 
             upper = str_extract(string = upper, pattern = "\\d{1,2}\\.\\d{1,2}|\\.\\d{1,2}|\\d")) %>% 
      mutate_at(c("OR", "lower", "upper"), as.numeric)
    
  } else if (dbl == FALSE & is_tibble(.data) == TRUE) { # NO contiene un signo de puntuación específico
    
    .data %>% 
      select({{ variables }}) %>% 
      mutate(tmp_chunks = str_split({{ icvar }}, pattern = {{ separator }}, n = 2)) %>%   
      mutate(lower = map_chr(tmp_chunks, 1), 
             upper = map_chr(tmp_chunks, 2)) %>% 
      select(-tmp_chunks, -{{ icvar }}) %>% 
      rename(characteristics = X1, 
             OR = {{ orvar }}) %>% 
      mutate(OR = str_extract(string = OR, pattern = "\\d{1,2}\\.\\d{1,2}|\\.\\d{1,2}|\\d")) %>% 
      mutate_at(c("OR", "lower", "upper"), as.numeric) 
    
  } else {
    warning("Check Data")
  }
} 


# Información del autor, año, y tipo de efecto RR u OR
tables_fin <- function(.data, author_year, or_rr = "OR") {
  require(tidyverse)
  
  .data %>% 
    mutate(author_year = {{ author_year }},
           or_rr = {{ or_rr }},
           characteristics = str_to_lower(characteristics))
 
}

