
################################################################################

# Título: Extracción de datos para meta análisis
# Proyecto: Factores asociados con el desarrollo de envejecimiento exitoso y 
#           saludable: Revisión Sistemática y metaanálisis de estudios 
#           longitudinales. 
# Autor: Carlos González-Carballo 
# email: carlos.gz.cb@gmail.com 
# Script: Extracción de la información de cada artículo a incluir en el metaanálisis
# Data: PDF de cara artículo incluido en el metaanálisis. 
# Copyright: Este Script es producto de Carlos González-Carballo como trabajo de
#            investigación del Doctorado en ciencias, Epidemiología.

# STATUS: Stable

################################################################################

# Stages
# 1: Extracción de la información. 
# 2:   

### ------------------------------------------------------------------- ###
###                           Artículos                                 ###
### ------------------------------------------------------------------- ###

# Andrews 2002 ------------------------------------------------------------

andrews <- pdf_text(pdf = here("Articulos", "Andrews (2002).pdf")) 

## Level of Function with Activity, Physical Performance and health --------
andrews_or_health <- andrews[11]
# 1
tab <- str_split(andrews_or_health, pattern = "\n")
  tab <- tab[[1]]
  tab <- tab[8:23] 
  tab <- tab[c(-2, -7:-8)]
# 2
andrews_or_health <- pdf_tables(tab)
# 3
andrews_or_health <- tables_extract(andrews_or_health, 
                                    variables = X1:X3, orvar = X2, icvar = X3, 
                                    separator = "–")
# 4
andrews_or_health <- andrews_or_health %>% 
  mutate(characteristics = recode(characteristics, 
                           "Domestic chores" = "Domestic chores (Adelaide)",
                           "Household maintenance" = "Household maintenance (Adelaide)",
                           "Service to others" = "Service to others (Adelaide)",
                           "Social activities"  = "Social activities (Adelaide)", 
                           "Moderate" = "Moderate exercise", 
                           "Vigorous" = "Vigorous exercise"))
tables_fin()

write.csv2(andrews_or_health, here("Data", "andrews2002_or_health.csv")) 

## Level of Function with Activity, Physical Performance and health --------
andrews_or_psych_cog <- andrews[12]
# 1 
tab <- str_split(andrews_or_psych_cog, pattern = "\n")
  tab <- tab[[1]]
  tab <- tab[c(8:11, 24:30)]
# 2
andrews_or_psych_cog <- pdf_tables(tab)
# 3
andrews_or_psych_cog <- tables_extract(andrews_or_psych_cog, 
                                       variables = X1:X3, orvar = X2, icvar = X3,
                                       separator = "–")
# 4 



















# otros datos 
per_h_ee <- (302/(302 + 223 + 253))*100
per_m_ee <- (201/(201 + 155 + 269))*100



