
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

## OR Healthy survival  ----------------------------------------------------
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
bell_healthy$characteristics[2] <- "BMI ≥25.0"
bell_healthy$characteristics[28] <- "Hemoglobin, g/dL >15"
bell_healthy$characteristics[19] <- ">15 onces/month (Alcohol)"

bell_healthy <- bell_healthy %>% 
  mutate(characteristics = recode(characteristics, 
                                  "<19.0" = "BMI <19.0", 
                                  "<120" = "Systolic blood pressure <120",
                                  ">160" = "Systolic blood pressure >160",  
                                  "Cognitive Abilities Screening Instrument" = "Cognitive Abilities Screening Instrument (74-81.9)", 
                                  "Center for Epidemiologic Studies Depression" = "Center for Epidemiologic Studies Depression 11-items (>9)", 
                                  "<13" = "Hemoglobin, g/dL <13", 
                                  "Past" = "Past smoker", 
                                  "Current" = "Current smoker", 
                                  "Never" = "Never (Alcohol)"))

# 5
bell_healthy <- tables_fin(bell_healthy, author_year = "Bell (2014)", or_rr = "OR")

write_csv(bell_healthy, here("Data", "bell2014_OR.csv"))

###---------------------------------------------------------------------###
# Britton (2008) ----------------------------------------------------------
britton <- pdftools::pdf_text(here("Articulos", "Britton (2008).pdf"))

## OR Successful aging -----------------------------------------------------
britton_success_a <- britton[4]
britton_success_b <- britton[5]

# 1 
tab_a <- str_split(britton_success_a, "\n")
  tab_a <- tab_a[[1]]
  tab_a <- tab_a[16:58]
tab_b <- str_split(britton_success_b, "\n")
  tab_b <- tab_b[[1]]
  tab_b <- tab_b[14:25]
  
# 2 
britton_success_a <- pdf_tables(tab_a)
britton_success_a <- subset(britton_success_a, subset = X2 !="" &
                              X1 != "P-value for trend" & 
                              X1 != "P-value" & 
                              X3 != "1.0")
britton_success_a[1:2, 6] <- britton_success_a[1:2, 5]

britton_success_b <- pdf_tables(tab_b)
britton_success_b <- subset(britton_success_b, subset = X2 !="" &
                              X1 != "P-value for trend" & 
                              X1 != "P-value" & 
                              X3 != "1.0")

britton_success <- bind_rows(britton_success_a, britton_success_b)

britton_success <- britton_success %>% select(X1, X3, X6) %>%
  mutate(tmp_chunks = str_split(X3, pattern = " ", n = 2)) %>%   
  mutate(OR_M = map_chr(tmp_chunks, 1), 
         IC_M = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks = str_split(X6, pattern = " ", n = 2)) %>%   
  mutate(OR_F = map_chr(tmp_chunks, 1), 
         IC_F = map_chr(tmp_chunks, 2)) %>% 
  select(-X3, - X6, -tmp_chunks) 
  
# Category add using str_c() and not recode() because possible errors.
# Many variables with same name.
britton_success$X1[1:2] <- str_c(britton_success$X1[1:2], "Employment grade", sep = " ")
britton_success$X1[3] <- str_c("Father social class", britton_success$X1[3], sep = " ")
britton_success$X1[4] <- "Age left education >18"
britton_success$X1[5] <- "Age left education 17-18"
britton_success$X1[6:7] <- str_c("Height tertile", britton_success$X1[6:7], sep = " ")
britton_success$X1[8] <- "Never Smoker"
britton_success$X1[10:11] <- str_c(britton_success$X1[10:11], "Alcohol, units/wk", sep = " ")
britton_success$X1[12] <- "No Poor diet"
britton_success$X1[13:14] <- str_c(britton_success$X1[13:14], "Physical activity", sep = " ")
britton_success$X1[15:16] <- str_c(britton_success$X1[15:16], "Decision latitude", sep = " ")
britton_success$X1[17:18] <- str_c(britton_success$X1[17:18], "Job demands", sep = " ")
britton_success$X1[19:20] <- str_c(britton_success$X1[19:20], "Work support", sep = " ") 
britton_success$X1[21:22] <- str_c(britton_success$X1[21:22], "Network Index", sep = " ")

### Hombres ----  
# 3 
britton_success_m <- tables_extract(britton_success, variables = c(X1, OR_M, IC_M), 
               orvar = OR_M, 
               icvar = IC_M, 
               separator = "–", 
               dbl = TRUE)
britton_success_m %>%  print(n = Inf)

# 4 already done
# 5 
britton_success_m <- tables_fin(britton_success_m, author_year = "Britton (2008)", or_rr = "OR")

write_csv(britton_success_m, here("Data", "britton2008_success_m.csv"))

### Mujeres ----
# 3
britton_success_f <- tables_extract(britton_success, variables = c(X1, OR_F, IC_F), 
                                    orvar = OR_F, 
                                    icvar = IC_F, 
                                    separator = "–", 
                                    dbl = TRUE)
britton_success_f %>%  print(n = Inf)

# 4 already done
# 5
britton_success_f <- tables_fin(britton_success_f, author_year = "Britton (2008)", or_rr = "OR")

write_csv(britton_success_f, here("Data", "britton2008_success_f.csv"))


## Otros datos -------------------------------------------------------------

per_hm <- 4140/5823*100
per_ee <- (548+246)/5823*100
per_t_disease <- (2549+988)/5823*100
per_t_function <- (757+361)/5823*100

# Ford (2000) -------------------------------------------------------------
ford <- pdftools::pdf_text(here("Articulos", "Ford (2000).pdf"))

## OR of sustained independence -------------------------------------------
ford_or <- ford[11]

# 1 
tab <- str_split(ford_or, "\n")
  tab <- tab[[1]]
  tab <- tab[12:31]
  tab <- tab[c(-7, -12, -16)]

# 2 
ford_or <- pdf_tables(tab)

# 3 
ford_or <- tables_extract(ford_or, variables = X1:X3, 
                          orvar = X2, icvar = X3, 
                          separator = "-")

# 4 Nombres no requieren de modificación
# 5 
ford_or <- tables_fin(ford_or, author_year = "Ford (2000)", or_rr = "OR")

write_csv(ford_or, here("Data", "ford2000_or.csv"))

## Otros datos ----
98/487*100 # % de personas exitosas 
n_mee <- round((49 * 98) / 100) # N de hombres exitosos 
n_mnee <- round((24.9 * 389) / 100) # N de hombres no exitosos 
n_fee <- 98 - n_mee # N de mujeres exitosas 
n_fnee <- 389 - n_mnee # N de mujeres no exitosas 

round(n_mee / (n_mnee + n_mee) * 100, digits = 2) # % de hombres exitosos
round(n_fee / (n_fnee + n_fee) * 100, digits = 2) # % de mujeres exitosas
