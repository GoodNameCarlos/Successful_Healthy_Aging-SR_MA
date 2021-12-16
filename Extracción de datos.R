
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

###---------------------------------------------------------------------###
# Andrews 2002 ------------------------------------------------------------
andrews <- pdftools::pdf_text(pdf = here("Articulos", "Andrews (2002).pdf")) 

## Level of Function with Activity, Physical Performance and health --------
andrews_or_phys_health <- andrews[11]
# 1
tab <- str_split(andrews_or_phys_health, pattern = "\n")
  tab <- tab[[1]]
  tab <- tab[8:23] 
  tab <- tab[c(-2, -7:-8)]
# 2
andrews_or_phys_health <- pdf_tables(tab)
# 3
andrews_or_phys_health <- tables_extract(andrews_or_phys_health, 
                                    variables = X1:X3, orvar = X2, icvar = X3, 
                                    separator = "–")
# 4
andrews_or_phys_health <- andrews_or_phys_health %>% 
  mutate(characteristics = recode(characteristics, 
                           "Domestic chores" = "Domestic chores (Adelaide)",
                           "Household maintenance" = "Household maintenance (Adelaide)",
                           "Service to others" = "Service to others (Adelaide)",
                           "Social activities"  = "Social activities (Adelaide)", 
                           "Moderate" = "Moderate exercise", 
                           "Vigorous" = "Vigorous exercise"))
# 5
andrews_or_phys_health <- tables_fin(andrews_or_phys_health, author_year = "Andrews 2002", or_rr = "OR")

write_excel_csv(andrews_or_phys_health, here("Data", "andrews2002_or_phys_health.csv")) 

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
andrews_or_psych_cog <- andrews_or_psych_cog %>%
  mutate(characteristics = recode(characteristics, 
                                  "Picture Immed." = "Picture Immed. (Memory)",
                                  "Picture Delay" = "Picture Delay (Memory)",
                                  "Symbol" = "Symbol (Cog)",
                                  "Speed" = "Speed (Cog)",
                                  "Naming" = "Naming (Cog)",
                                  "NART errors" = "NART errors",
                                  "Similarities" = "Similarities (Cog)"))
# 5
andrews_or_psych_cog <- tables_fin(andrews_or_psych_cog, author_year = "Andrews (2002)", or_rr = "OR")

write_excel_csv(andrews_or_psych_cog, here("Data", "andrews2002_or_psych_cog.csv"))

## Level of function by medical conditions ---------------------------------
andrews_or_health <- andrews[8]
# 1 
tab <- str_split(andrews_or_health, "\n")
  tab <- tab[[1]]
  tab <- tab[39:49]
# 2
andrews_or_health <- pdf_tables(tab)
# 3
andrews_or_health <- tables_extract(andrews_or_health, 
                                    variables = X1:X3, orvar = X2, icvar = X3, 
                                    separator = "–")
# 4 (No rename)
# 5 
andrews_or_health <- tables_fin(andrews_or_health, author_year = "Andrews (2002)", or_rr = "OR")

write_excel_csv(andrews_or_health, here("Data", "andrews2002_or_health.csv"))


## Otros datos ------------------------------------------------------------

per_h_ee <- (302/(302 + 223 + 253))*100
per_m_ee <- (201/(201 + 155 + 269))*100

###---------------------------------------------------------------------###
# Arroyo Quiroz (2020) ----------------------------------------------------
arroyo_quiroz <- pdftools::pdf_text(pdf = here("Articulos", "Arroyo-Quiroz (2020).pdf"))

## HR of losing Healthy Aging Status ---------------------------------------
arroyo_quiroz_HR <- arroyo_quiroz[15]
# 1
tab <- str_split(arroyo_quiroz_HR, "\n")
  tab <- tab[[1]]
  tab <- tab[c(17:25, 31:37)]
  tab <- tab[c(-5, -8)]

# 2
arroyo_quiroz_HR <- pdf_tables(data = tab)
# 3 
arroyo_quiroz_HR <- tables_extract(arroyo_quiroz_HR, 
                                   variables = X1:X3, orvar = X2, icvar = X3, 
                                   separator = ",", dbl = TRUE)
# 4
arroyo_quiroz_HR <- arroyo_quiroz_HR %>% 
  mutate(characteristics = recode(characteristics, 
                                  "Former" = "Former smoker",
                                  "Current" = "Current smoker"))
# 5
arroyo_quiroz_HR <- tables_fin(arroyo_quiroz_HR, author_year = "Arroyo-Quiroz (2020)", or_rr = "HR")

write_csv(arroyo_quiroz_HR, here("Data", "arroyo_quiroz2020_HR.csv"))


## OR of HA at age 77 ------------------------------------------------------
arroyo_quiroz_OR77 <- arroyo_quiroz[16]
# 1 
tab <- str_split(arroyo_quiroz_OR77, pattern = "\n")
  tab <- tab[[1]]
  tab <- tab[c(16:24, 30:36)]
  tab <- tab[-7]
  
# 2
arroyo_quiroz_OR77 <- pdf_tables(tab)
# 3 
arroyo_quiroz_OR77 <- tables_extract(arroyo_quiroz_OR77, 
                                     variables = c(X1, X2, X4), orvar = X2, icvar = X4,
                                     separator = ",", dbl = T)
  ## ajustado
  arroyo_quiroz_OR77_adj <- tables_extract(arroyo_quiroz_OR77, 
                                       variables = c(X1, X5, X7), orvar = X5, icvar = X7,
                                       separator = ",\\s|,", dbl = T)
# 4 (No rename)
# 5
arroyo_quiroz_OR77 <- tables_fin(arroyo_quiroz_OR77, author_year = "Arroyo-Quiroz (2020)", or_rr = "OR")

write_csv(arroyo_quiroz_OR77, here("Data", "arroyo2020_quiroz_OR77.csv"))

## OR of HA at age 90 ------------------------------------------------------
arroyo_quiroz_OR90 <- arroyo_quiroz[17]
# 1 
tab <- str_split(arroyo_quiroz_OR90, pattern = "\n")
  tab <- tab[[1]]
  tab <- tab[c(16:25, 31:37)]
  tab <- tab[c(-7:-8, -10)]

# 2
arroyo_quiroz_OR90 <- pdf_tables(tab)
# 3 
arroyo_quiroz_OR90 <- tables_extract(arroyo_quiroz_OR90, 
                                     variables = c(X1, X2, X4), orvar = X2, icvar = X4,
                                     separator = ",", dbl = T)
  ## ajustado
  tables_extract(arroyo_quiroz_OR90, 
                 variables = c(X1, X5, X7), orvar = X5, icvar = X7,
                 separator = ",", dbl = T)
# 4 (No rename)
# 5
arroyo_quiroz_OR90 <- tables_fin(arroyo_quiroz_OR90, author_year = "Arroyo-Quiroz (2020)", or_rr = "OR")

write_csv(arroyo_quiroz_OR90, here("Data", "arroyo2020_quiroz_OR90.csv"))


###---------------------------------------------------------------------###
# Bell (2014) -------------------------------------------------------------
bell <- pdftools::pdf_text(pdf = here("Articulos", "Bell (2014).pdf"))

# OR Healthy survival  ----------------------------------------------------
bell_healthy <- bell[5]
# 1
tab <- str_split(bell_healthy, "\n")
  tab <- tab[[1]]
  tab <- tab[12:51]
  tab <- tab[c(-3, -4, -11, -17, -20, -22, -29, -32:-33, -36)]
# 2
bell_healthy <- pdf_tables(tab)
bell_healthy %>% print(n = Inf)
  ## arreglo de data  
  bell_healthy_temp <- bell_healthy %>%
    slice(c(1:2, 14:16, 23:26)) %>% select(X1, X3) %>% 
    mutate(X = str_split(X3, pattern = "\\s", n = 3)) %>% 
    mutate(X5 = map_chr(X, 1), 
           X6 = map_chr(X, 2)) %>% 
    select(-X3, -X) 
  bell_healthy <- bell_healthy %>% slice(-c(1:2, 14:16, 23:26)) %>% bind_rows(bell_healthy_temp)
  rm(bell_healthy_temp)
# 3
bell_healthy <- tables_extract(.data = bell_healthy, variables = c(X1, X5, X6), orvar = X5, icvar = X6, separator = "–", dbl = TRUE)

# 4
bell_healthy %>% 
  mutate(characteristics = rename(characteristics, 
                                  <19.0 = , 
                                  =25.0 = , 
                                  Waist–hip ratio >0.99 = , 
                                  Forced expiratory volume in 1 second <2.1 L = , 
                                  Walk speed =0.75 m/sa = , 
                                  Grip strength <33 kg = , 
                                  <120 = , 
                                  >160 = , 
                                  Seated diastolic blood pressure >90 mmHg = , 
                                  Hypertension = , 
                                  Cognitive Abilities Screening Instrument = , 
                                  Triglycerides =150 mg/dL = , 
                                  High-density lipoprotein cholesterol <40 mg/dL = , 
                                  Glucose =126 mg/dL = , 
                                  Insulin =20 lU/mL = , 
                                  Fibrinogen >351 mg/dL = , 
                                  White blood cell count >6,000 cells/lL = , 
                                  Never = ,  
                                  >15 = , 
                                  Physical Activity Index =30.4 = , 
                                  Blocks walked per day <12 = , 
                                  Education <12 years = ,  
                                  Unmarried in late life = , 
                                  Ankle–brachial index <0.9 = , 
                                  Center for Epidemiologic Studies Depression = , 
                                  Fair or poor self-rated health = , 
                                  <13 = , 
                                  >15 = , 
                                  Past = , 
                                  Current = , ))
  
