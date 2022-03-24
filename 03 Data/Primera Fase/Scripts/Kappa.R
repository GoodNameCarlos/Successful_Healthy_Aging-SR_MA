#------------------------------------------------------------------------------#

# Título: Coeficiente de Kappa
# Proyecto: Factores asociados con el desarrollo de envejecimiento exitoso y 
#           saludable: Revisión Sistemática y metaanálisis de estudios 
#           longitudinales. 
# Autor: Carlos González-Carballo 
# email: carlos.gz.cb@gmail.com 
# Script: Análisis de concordancia entre evaluadores. 
# Data: Bases de datos de análisis de cada evaluador del proyecto. 
# Copyright: Este Script es producto de Carlos González-Carballo como trabajo de
#            investigación del Doctorado en ciencias, Epidemiología.

# STATUS: Stable

#------------------------------------------------------------------------------#

# install.packages("irr")
# install.packages("psych")
library(irr)
library(psych)
library(here)
library(tidyverse)

# Sample size: Kappa ------------------------------------------------------

# Debemos iniciar definiendo el nivel de Kappa que sería nuestra H0. Puede ser que  
# sea de 0.4 en el límite superior, de lo que sería inaceptablemente bajo. Por lo que
# tenemos que definir qué sería el nivel de concordancia aceptable. 
# Puede ser que sea un nivel de 0.75 el valor bajo, lo que sería considerado concordancia "substancial"
# Y finalmente, se tiene que tener una idea de la prevalencia del la característica a medir. 

# ejemplo, podemos querer probar una H0 de kappa == 0.75, e HA == > 0.75. 
# donde kappa sea == 0.90 y los dos evaluadores puedan classificar al 10% de los componentes como positivos. 

N.cohen.kappa(0.30, 0.30, k1 = 0.85, k0 = .75)

#  Data --------------------------------------------------------------------
data_carlos <- read_csv(here("Data", "DF_articulo.csv"))
data_adrian <- readxl::read_excel(here("Data", "Base de datos_Articulos_rev_meta_EE_ExcelFormat.xlsx"))

data_carlos$name <- "Carlos"
data_adrian$name <- "Adrian"

data_carlos <- rename(data_carlos, fecha = "...1")  # Var 1 name change 

  # Extraction of the titles in Adrian's database to delete entries. 
  titles <- data_adrian$titulo
  data_carlos <- data_carlos[data_carlos$titulo %in% titles,] # Alternatively: is.element()

## Merge data bases ---- 

data_rev <- bind_rows(data_carlos[, c(2, 8:17)], data_adrian[, c(2, 8:17)])
  
data_rev <- data_rev %>% 
  mutate(accion_2 = ifelse(edad_45 == 1 &
                             factores_ee == 1 &
                             envejecimiento_es == 1 &
                             cohorte %in% c(1,2,3),
                           1, 0))  

data_kappa <- data_rev %>% 
  pivot_wider(names_from = name, values_from = c(2:12)) %>% 
  select(!c("name_Adrian", "name_Carlos"))

## Agreement ----
data_kappa <- data_kappa %>% 
  mutate(agree = case_when(accion_2_Carlos == 1 & accion_2_Adrian == 1 | 
                             accion_2_Carlos == 0 & accion_2_Adrian == 0 ~ "agree",
                           accion_2_Carlos == 1 & accion_2_Adrian == 0 | 
                             accion_2_Carlos == 0 & accion_2_Adrian == 1 ~ "disagree"))  

irr::agree(data_kappa[, 20:21])
  questionr::freq(data_kappa$agree) # Agreement per variable

  # Adrian inter agreement
  adrian_agree <- data_kappa %>% 
    select(accion_Adrian) %>% 
    mutate(accion_Adrian_mutate = ifelse(accion_Adrian == 2, 0, 1), 
           agree = ifelse(accion_Adrian == accion_Adrian_mutate, "agree", "disagree")) 
  
  questionr::freq(adrian_agree$agree)
    irr::agree(adrian_agree[, 1:2])

## Disagreement data base ----
  data_disagreement <- data_kappa %>% dplyr::filter(agree == "disagree")
  write_excel_csv(data_disagreement, here("Data", "DF_articulo_disagreement.csv")) # disagreemnt by raters Carlos & Adrian, missing Etna's. 

## Agreement data base ----
  data_agreement <- data_kappa %>% dplyr::filter(agree == "agree")
  write_excel_csv(data_agreement, here("Data", "DF_articulo_agreement.csv"))
  
# Kappa Statistic ---------------------------------------------------------
####
# Supuestos y requisitos: 
#   1. Dos variables de resultado categ?ricas, pueden ser ordinales o nominales.   
#   2. Las dos variables deben tener exactamente la misma categoria. 
#   3. Tenemos observaciones pareadas; cada elemento est? categorizados dos veces por dos evaluadores independientes. 
#   4. Se utilizan los mismos evaluadores para todos los participantes. 
     
psych::cohen.kappa(data_kappa[, 20:21])

# Kappa 
irr::kappa2(data_kappa[, 20:21])
irr::kendall(data_kappa[, 20:21])

# Kappa de cada elemento
  irr::kappa2(data_kappa[, 2:3])
  irr::kappa2(data_kappa[, 4:5])
  irr::kappa2(data_kappa[, 6:7])
  irr::kappa2(data_kappa[, 8:9])
  irr::kappa2(data_kappa[, 10:11])
  irr::kappa2(data_kappa[, 12:13])
  irr::kappa2(data_kappa[, 14:15])


# Kappa Example --------------------------------------------------------------

diagnoses <- as.table(rbind(
  c(7, 1, 2, 3, 0), c(0, 8, 1, 1, 0),
  c(0, 0, 2, 0, 0), c(0, 0, 0, 1, 0),
  c(0, 0, 0, 0, 4)))

categories <- c("Depression", "Personality Disorder",
                "Schizophrenia", "Neurosis", "Other")

dimnames(diagnoses) <- list(Doctor1 = categories, Doctor2 = categories)
diagnoses

psych::cohen.kappa(x = diagnoses)
irr::kappam.light(diagnoses[, 1:3])



