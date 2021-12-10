
### ------------------------------------------------------------------------ ###
# Título: Función para la extracción de datos
# Proyecto: Factores asociados con el desarrollo de envejecimiento exitoso y 
#           saludable: Revisión Sistemática y metaanálisis de estudios 
#           longitudinales.
# Autor: Carlos González-Carballo 
# email: carlos.gz.cb@gmail.com 
# Script: Función para la extracción de datos de PDF.
# Data: datos de PDF para metaanálisis. 
# Copyright: Este Script es producto de Carlos González-Carballo como trabajo de
#            investigación del Doctorado en ciencias, Epidemiología.
### ------------------------------------------------------------------------ ###
#                     STATUS: Stable
### ------------------------------------------------------------------------ ###

# Paquetes
library(tidyverse)
library(here)
library(pdftools)
library(purrr)

# Programación con Tidiverse
# https://dplyr.tidyverse.org/articles/programming.html

# Función para la extracción ----------------------------------------------

# Pasos: 
# • Obtención de la información del pdf. 
# • Extracción de datos: Se obtienen los datos del pdf. 
# • Extracción de nombres de cada columna. 
# • Data.frame: Se genera la base de datos del artículo. 

## Preparación de la información 
# Se usa Andrews (2002) como prueba
andrews <- pdf_text(pdf = here("Articulos", "Andrews (2002).pdf")) 

# Extracción de la hoja en la que se encuentra la tabla
andrews_AOR_health <- andrews[11]

# Extracción de la tabla
tab <- str_split(andrews_AOR_health, pattern = "\n")
  tab <- tab[[1]]

  # se seleccionan las líneas en las que se encuentra la información
  table <- tab[8:23]
  table # las líneas 2 y 7 no contienen información relevante 
  table <- table[c(-2, -7)] # Se eliminan  
  
# Creando función para la creación del data.frame

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

pdf_tables(table)

andrews <- pdf_tables(table)

# Se elimina cualquier otro dato que no sea importante para la extracción 
andrews <- andrews[-6,]

# Se extraen los valores numéricos

tables_extract_dbl <- function(.data, variables, separator, icvar) {
  require(stringr)
  require(tidyverse)
    
  if (is_tibble(.data) == TRUE) {
  .data %>% 
    select({{ variables }}) %>% 
    mutate(tmp_chunks = str_split({{ icvar }}, fixed({{ separator }}), n = 2)) %>%   
    mutate(lower = map_chr(tmp_chunks, 1), 
           upper = map_chr(tmp_chunks, 2)) %>% 
    select(-tmp_chunks, -{{ icvar }}) %>% 
    rename(characteristics = X1, 
           OR = X2) %>% 
    mutate(OR = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}")) %>% 
    mutate_at(c("OR", "lower", "upper"), as.numeric) 
  
    } else {
    warning("check data")
  }
}

andrews <- tables_extract_dbl(andrews, X1:X3, separator = "–", icvar = X3)
          
          # Referencia 
          andrews %>% 
            select(X1:X3) %>% 
            mutate(tmp_chunks = str_split(X3, fixed("–"), n = 2)) %>% 
            mutate(lower = map_chr(tmp_chunks, 1), 
                   upper = map_chr(tmp_chunks, 2)) %>% 
            select(-tmp_chunks, -X3) %>% 
            rename(characteristics = X1, 
                   OR = X2) %>% 
            mutate(OR = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}")) %>% 
            mutate_at(c("OR", "lower", "upper"), as.numeric)

# Se agregan la informaciond el autor, tipo de efecto, y cambio a minuculas
tables_fin <- function(.data, ) {
  
}
          # Referencia
          andrews <- andrews %>% 
            mutate(author_year = "Britton (2008)",
                   or_rr = "OR",
                   characteristics = str_to_lower(characteristics))

# Producto final 
andrews


# El problema con puntuaciones: Arroyo Quiroz ---------------------------------------------
arroyo_quiroz <- pdf_text(pdf = here("Articulos", "Arroyo-Quiroz (2020).pdf"))
arroyo_quiroz_HR <- arroyo_quiroz[15]
  tab <- str_split(arroyo_quiroz_HR, "\n")
  tab <- tab[[1]]
  table <- c(tab[17:25], tab[31:37]) 
  table <- table[c(-5, -8)]
  
arroyo_quiroz <- pdf_tables(table)

arroyo_quiroz

## el problema es que aquí se transforma en double 
tables_extract_dbl(arroyo_quiroz, X1:X3, ",", icvar = X3) 

## Se genera la función para responder a esa problemática 
tables_extract_punct <- function(.data, variables, separator, IC_VAR) {
  require(tidyverse) 
  require(stringr)
  
  if (is_tibble(.data) == TRUE) {
    .data %>% 
      select({{ variables }}) %>% 
      mutate(tmp_chunks = str_split({{ IC_VAR }}, fixed({{ separator }}), n = 2)) %>%   
      mutate(lower = map_chr(tmp_chunks, 1), 
             upper = map_chr(tmp_chunks, 2)) %>% 
      select(-tmp_chunks, -{{ IC_VAR }}) %>% 
      rename(characteristics = X1, 
             OR = X2) %>% 
      mutate(OR = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}"), 
             lower = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}"), 
             upper = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}")) %>% 
      mutate_at(c("OR", "lower", "upper"), as.numeric)
  } else {
    warning("check data")
  }
}

tables_extract_punct(.data = arroyo_quiroz, variables = X1:X3, separator = ",", IC_VAR = X3)


# Function "tables_extract" complete --------------------------------------
# chequeo de la función "str_detect" con tabble y tidyverse
test <- arroyo_quiroz %>% summarise(check = all(str_detect(string = X3, pattern = "[[:punct:]]")))

test == F

# prueba 

Prueba <- function(.data, icvar, punct) {
  require(tidyverse)
  require(stringr)
  
  # Chequeo de si contiene signos de puntuación 
  check <- .data %>% summarise(check = all(str_detect(string = {{ icvar }}, pattern = {{ punct }})))
  
  if (check == TRUE) { 
    print("Contains a punct")
  } else if (check == FALSE) {
      print("Does not contain a punct")
  } else {
      warning("Does not work")
    }
}

Prueba(arroyo_quiroz, icvar = X3, punct = "\\(")
Prueba(andrews, icvar = X3, punct = "\\(")

# Conjuto de las funciones pasadas en una sola

tables_extract <- function(.data, variables, separator, icvar, punct) {
  require(tidyverse)
  require(stringr)
  
  # Chequeo de si contiene signos de puntuación 
  check <- .data %>% summarise(check = all(str_detect(string = {{ icvar }}, pattern = {{ punct }})))
  
  # Si contiene un signo de puntuación específico
  if (check == TRUE & is_tibble(.data) == TRUE) { 
    
    .data %>% 
      select({{ variables }}) %>% 
      mutate(tmp_chunks = str_split({{ IC_VAR }}, fixed({{ separator }}), n = 2)) %>%   
      mutate(lower = map_chr(tmp_chunks, 1), 
             upper = map_chr(tmp_chunks, 2)) %>% 
      select(-tmp_chunks, -{{ IC_VAR }}) %>% 
      rename(characteristics = X1, 
             OR = X2) %>% 
      mutate(OR = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}"), 
             lower = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}"), 
             upper = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}")) %>% 
      mutate_at(c("OR", "lower", "upper"), as.numeric)
    
  } else if (check == FALSE) { # Si no contiene un signo de puntuación específico
    print("Does not contain a punct")
  } else {
    warning("Does not work")
  }
} 


# Deprecated functions ----------------------------------------------------

tables_extract_punct_Deprecated <- function(.data, variables, separator, IC_VAR, punct1, punct2) {
  if (is_tibble(.data) == TRUE) {
    .data %>% 
      select({{ variables }}) %>% 
      mutate(tmp_chunks = str_split({{ IC_VAR }}, fixed({{ separator }}), n = 2)) %>%   
      mutate(lower = map_chr(tmp_chunks, 1), 
             upper = map_chr(tmp_chunks, 2)) %>% 
      select(-tmp_chunks, -{{ IC_VAR }}) %>% 
      rename(characteristics = X1, 
             OR = X2) %>% 
      mutate(OR = str_extract(string = OR, pattern = "^\\d{1,2}.\\d{1,2}|.\\d{1,2}"), 
             lower = str_remove(string = lower, pattern = {{ punct1 }}), 
             upper = str_remove(string = lower, pattern = {{ punct2 }})) %>% 
      mutate_at(c("OR", "lower", "upper"), as.numeric)
  } else {
    warning("check data")
  }
}






























