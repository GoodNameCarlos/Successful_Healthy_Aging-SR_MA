
# library(tidyverse) 
# library(dbplyr)
# library(stringr)
# Los paquetes en los comentario est?n precargados en PC original
library(here)
library(rio)

# Base de datos -----------------------------------------------------------

# Importando la base de datos 

DF_articulos_original <- read_csv(here("03 Data", "Original", "20210830_DT Artículos(NoMod)_06Datos_1247.csv"))

DF_articulos_org <- as_tibble(DF_articulos_original)

class(DF_articulos_org)

head(DF_articulos_org)

DF_articulo <- DF_articulos_org %>% 
  select(Title, Author, "Publication Year", DOI, "Publication Title") 

  rm(DF_articulos_original)
  rm(DF_articulos_org)


# Exportar datos originales -----------------------------------------------

write_excel_csv(DF_articulo, here("03 Data", "20210830_DT Artículos limpio_06Datos.csv"))

# Limpieza ----------------------------------------------------------------

#Se extrae el nombre del primer autor. 
head(DF_articulo)
head(DF_articulo$Author)

DF_articulo$autor <- word(DF_articulo$Author, 1)

DF_articulo %>% 
  select(Author, autor) %>% 
  head(10)

## Elimiaci?n de strings no deseados ---------------------------------------
# Se elimina la coma ","
DF_articulo$autor <- gsub(pattern = "," , replacement = "", DF_articulo$autor)
head(DF_articulo$autor, 20)

  # Se eliminan elementos no deseados en los t?tulos de los art?culos. En espec?fico: [] y "". 
      # ejemplo de como se debe hacer
      s <- c("[Dave]", "[Tony]", "[Sara]")
      gsub("\\[|\\]", "", s)
      rm(s)

# Back slash  
str_detect(DF_articulo$Title, "\\[|\\]") %>% sum() # 36 titulos con []

  titulos_r <- DF_articulo$Title[str_detect(DF_articulo$Title, "\\[|\\]")]
  export(as.matrix(titulos_r), "Nombres.csv")
  noquote(gsub("\\[|\\]", "", titulos_r))

DF_articulo$Title <- gsub("\\[|\\]", "", DF_articulo$Title)
  str_detect(DF_articulo$Title, "\\[|\\]") %>% sum() # da cero

# Quotes
# str_detect(DF_articulo$Title, "\"") %>% sum() # 10 t?tulos con ""
# 
#   titulos_q <- DF_articulo$Title[str_detect(DF_articulo$Title, "\"")] %>% noquote()
#   titulos_q
#   gsub("\"", "", titulos_q)
  
# NOTA: Se elimina esta secci?n por que s?lo es un art?culo que genera conflicto, por lo que se decide modificarlo a mano. 

## Incluisi?n de variables -------------------------------------------------

# Se reemplazan los nombres de la variables

DF_articulo <- DF_articulo %>% 
  rename(titulo = Title, co_autores = Author, ano_pub = "Publication Year", revista = "Publication Title")

# Se agrega la variables de evaluaci?n (edad_45, factores_ee, reporte_rr, reporte_or, envejecimiento_es, cohorte)

DF_articulo <- DF_articulo %>% add_column(fecha = NA, 
                                          edad_45 = NA, 
                                          factores_ee = NA, 
                                          reporte_rr = NA, 
                                          reporte_or = NA, 
                                          envejecimiento_es	= NA, 
                                          cohorte = NA,
                                          estudio_ee = NA,
                                          accion = NA, 
                                          comentario = NA) 

# Se da orden a las variables y sus tipos
DF_articulo <- relocate(DF_articulo, fecha)
DF_articulo <- relocate(DF_articulo, autor, .after = titulo)

# Exportaci?n -----------------------------------------------------------

write_excel_csv(DF_articulo,  here("03 Data", "DF_articulo.csv"))

