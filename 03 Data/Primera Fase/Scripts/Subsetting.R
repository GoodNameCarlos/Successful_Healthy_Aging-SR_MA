# Revisión sistemática: Envejecimiento exitoso ----------------------------

library(here)
library(tidyverse)
library(lubridate)
library(questionr)

# Importando la base de datos

DF_articulo <- read_csv(here("Data", "DF_articulo.csv"))  

DF_articulo

  spec(DF_articulo)
  
# Limpieza de la base de datos
# fecha a date
DF_articulo <- rename(DF_articulo, fecha = ...1)
  DF_articulo$fecha <- dmy(DF_articulo$fecha) 

# Bases de datos ----------------------------------------------------------

##
## Segunda etapa  ----------------------------------------------------------
##
  
DF_articulo_select <- DF_articulo %>% 
  mutate(accion_2 = ifelse(edad_45 == 1 & 
                             factores_ee == 1 & 
                             envejecimiento_es == 1 & 
                             cohorte %in% c(1,2,3), 1, 2)) %>% 
  dplyr::filter(accion_2 == 1) %>% 
    select(titulo, autor, co_autores, ano_pub, DOI, revista)
    
write_csv(DF_articulo_select, here("Data", "DF_articulo_2dafase.csv") )

  # sin datos de selección
  doi <- DF_articulo_select %>% 
    select(DOI)
  
  write_csv(doi, here("Data", "doi.csv"))

## 
## Artculos en duda -------------------------------------------------------
##

DF_articulo_dudoso <- DF_articulo %>% 
  dplyr::filter(accion == 3) %>% 
  select(titulo, autor, co_autores, ano_pub, DOI, revista)

  ###
  # Eliminar dulpicados de seleccionados
  ###

  DF_articulo_select$titulo %in% DF_articulo_dudoso$titulo %>% sum()
  
  repetidos <- DF_articulo %>% 
    mutate(select = ifelse(edad_45 == 1 & 
                              factores_ee == 1 & 
                              envejecimiento_es == 1 & 
                              cohorte %in% c(1,2,3), 1, 0), 
           dudoso = ifelse(accion == 3, 1, 0), 
           rep = select + dudoso) %>%  
    filter(rep == 2) %>% 
    pull(titulo)

  DF_articulo_dudoso2 <- DF_articulo_dudoso %>% 
    mutate(repetido = ifelse(DF_articulo_dudoso$titulo %in% repetidos, 1, 2)) %>% 
    filter(repetido == 2) %>% 
    select(!c(repetido))
    
write_csv(DF_articulo_dudoso2, here("Data", "DF_articulo_dudoso.csv")) # 62 articulos en duda 

##
## Articulos de proyectos de Envejecimiento  -------------------------------
##

DF_articulo %>% 
  dplyr::filter(estudio_ee == 1) %>% 
  nrow()

proyecto <- DF_articulo %>%
  dplyr::filter(estudio_ee == 1) %>% 
  select(titulo, co_autores, ano_pub, DOI) %>% 
  mutate(estudio = NA) 

write_csv(proyecto, here("Data", "Proyecto.csv"))

##
## Descriptivos ------------------------------------------------------------
##

DF_articulo <- DF_articulo %>% 
  mutate(accion_2 = ifelse(edad_45 == 1 & 
                             factores_ee == 1 & 
                             envejecimiento_es == 1 & 
                             cohorte %in% c(1,2,3),
                           1, 2))

freq(DF_articulo$accion_2)

##
# Muestra:: Kappa ---------------------------------------------------------
##

#
sample_a <- DF_articulo %>% 
  dplyr::filter(accion_2 == 1) 

#
set.seed(3118121519)

sample_b <- DF_articulo %>% 
  dplyr::filter(accion_2 == 2 & ano_pub >= 2010) %>% 
  slice_sample(n = 198)

kappa <- bind_rows(sample_a, sample_b)

kappa <- kappa %>% 
  mutate(fecha = NA, 
         edad_45 = NA, 
         factores_ee = NA,
         reporte_rr = NA, 
         reporte_or = NA, 
         envejecimiento_es = NA, 
         estudio_ee = NA,
         cohorte = NA,
         accion = NA, 
         comentario = NA) %>% 
  select(!c(accion_2)) 

kappa

write_csv(kappa, here("Data", "kappa.csv"))


