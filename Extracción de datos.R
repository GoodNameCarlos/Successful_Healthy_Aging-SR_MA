
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
#                 STATUS: Deprecated
### ------------------------------------------------------------------------ ###

# Paquetes ----------------------------------------------------------------
library(here)
library(pdftools)
library(tidyverse)

# Extracción de datos de cada artículo ------------------------------------

# pasos: 
  # • Obtención de la información del pdf. 
  # • Extracción de datos: Se obtienen los datos del pdf. 
  # • Extracción de nombres de cada columna. 
  # • Data.frame: Se genera la base de datos del artículo. 

# Extracción de datos 
# Extracción de nombres 
# Data.frame

### ------------------------------------------------------------------- ###
###                           Artículos                                 ###
### ------------------------------------------------------------------- ###

### ------------------------------------------------------------------- ###
# Andrews (2002) ----------------------------------------------------------
### ------------------------------------------------------------------- ###
andrews <- pdf_text(pdf = here("Articulos", "Andrews (2002).pdf")) 

per_h_ee <- (302/(302 + 223 + 253))*100
per_m_ee <- (201/(201 + 155 + 269))*100
  
## Level of Function with Activity, Physical Performance and health indicators -----------------
andrews_AOR_health <- andrews[11]

# Extracción de datos 
tab <- str_split(andrews_AOR_health, pattern = "\n")
  tab <- tab[[1]]
  table <- tab[8:23]
  table <- table[c(-2, -7)]

# Extracción de nombres 
names <- tab[7]
names
  names <- str_c("characteristics", names)
  names <- names %>% 
    str_trim() %>% 
    str_split("\\s{2,}", simplify = T)
  names <- names[-4:-5]
  names <- matrix(names, nrow = 1, ncol = 3)

# Data.frame
andrews_AOR_health <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>%  
  select("X1":"X3") %>% 
  setNames(names) %>% 
  as_tibble()
  
andrews_AOR_health <- andrews_AOR_health %>% 
  separate(col = `95% CI`, into = c("lower", "upper"), sep = "–")

andrews_AOR_health <- andrews_AOR_health %>% 
  mutate(AOR = str_extract(string = AOR, pattern = "^\\d.\\d{2}|^.\\d{2}")) %>% 
  dplyr::filter(characteristics != "None") %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Andrews (2002)", 
         or_rr = "OR", 
         characteristics = recode(characteristics, 
                                  "Domestic chores" = "Domestic chores (Adelaide)",
                                  "Household maintenance" = "Household maintenance (Adelaide)",
                                  "Service to others" = "Service to others (Adelaide)",
                                  "Social activities"  = "Social activities (Adelaide)", 
                                  "Moderate" = "Moderate exercise", 
                                  "Vigorous" = "Vigorous exercise"), 
         characteristics = str_to_lower(characteristics))

andrews_AOR_health

write_csv(andrews_AOR_health, here("Data", "andrews_AOR_health_2002.csv"))

## Level of Function with Sense of Self Variables ------------------------------
andrews_AOR_psych <- andrews[12]

# Extracción de datos 
tab <- str_split(andrews_AOR_psych, "\n")
tab <- tab[[1]]
table <- tab[8:11]

# Extracción de nombres 
# Los mismos que Level of Function with Activity, Physical Performance and health indicators
  warning("Same names as andrews_AOR_health--line 38")

# Data.frame
andrews_AOR_psych <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F)  %>%  
  select("X1":"X3") %>% 
  setNames(names) %>%   
  as_tibble()

andrews_AOR_psych <- andrews_AOR_psych %>% separate(col = `95% CI`, 
                                                    into = c("lower", "upper"),
                                                    sep = "–")

andrews_AOR_psych <- andrews_AOR_psych %>% 
  mutate(AOR = str_extract(string = AOR, pattern = "^\\d.\\d{2}|^.\\d{2}")) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Andrews (2002)", 
         or_rr = "OR", 
         characteristics = str_to_lower(characteristics))

andrews_AOR_psych

write_csv(andrews_AOR_psych, here("Data", "andrews_AOR_psych_2002.csv"))

## Level of Function with Cognitive Variables ----------------------------------
andrews_AOR_cog <- andrews[12]

# Extracción de datos 
tab <- str_split(andrews_AOR_cog, "\n")
tab <- tab[[1]]
table <- tab[24:30]
table
# Extracción de nombres 
  # Los mismos que Level of Function with Activity, Physical Performance and health indicators
  warning("Same names as andrews_AOR_health--line 38")

# Data.frame
andrews_AOR_cog <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F)  %>%  
  select("X1":"X3") %>% 
  setNames(names) %>%   
  as_tibble()

andrews_AOR_cog <- andrews_AOR_cog %>% separate(col = `95% CI`, 
                                                    into = c("lower", "upper"),
                                                    sep = "–")

andrews_AOR_cog <- andrews_AOR_cog %>% 
  mutate(AOR = str_extract(string = AOR, pattern = "^\\d.\\d{2}|^.\\d{2}")) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Andrews (2002)", 
         or_rr = "OR", 
         characteristics = recode(characteristics, 
                                  "Picture Immed." = "Picture Immed. memory", 
                                  "Picture Delay" = "Picture Delay memory",  
                                  "Symbol" = "Symbol memory"), 
         characteristics = str_to_lower(characteristics)) 

andrews_AOR_cog

write_csv(andrews_AOR_cog, here("Data", "andrews_AOR_cog_2002.csv"))

### ------------------------------------------------------------------- ###
# Arroyo-Quiroz (2020) ----------------------------------------------------
### ------------------------------------------------------------------- ###
arroyo_quiroz <- pdf_text(pdf = here("Articulos", "Arroyo-Quiroz (2020).pdf"))

## HR for losing HA status -------------------------------------------------
arroyo_quiroz_HR <- arroyo_quiroz[15]

# Extracción de datos 
tab <- str_split(arroyo_quiroz_HR, "\n")
tab <- tab[[1]]
table <- c(tab[17:25], tab[31:37]) 
table <- table[c(-5, -8)]

# Extracción de nombres de cada columna
names <- tab[16] 
names
names <- names %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T)
names[1,1] <- "Basal characteristics"  
names <- names[c(-2:-3, -6)]
names <- matrix(names, nrow = 1, ncol = 3)

# Data.frame
arroyo_quiroz_HR <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>% 
  select("X1", "X5", "X6") %>% 
  setNames(names) %>% 
  as_tibble()
  
arroyo_quiroz_HR <- arroyo_quiroz_HR %>% separate(col = `95% CI`,
                                                  into = c("lower", "upper"), 
                                                  sep = ",")

arroyo_quiroz_HR <- arroyo_quiroz_HR %>% 
  mutate(lower = str_remove(lower, pattern = "\\("), 
         upper = str_remove(upper, pattern = "\\)")) %>% 
  mutate_at(c("HR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Arroyo-Quiroz (2020)",
         or_rr = "HR",
         `Basal characteristics` = recode(`Basal characteristics`, 
                                          "Former" = "former smoker", 
                                          "Current" = "current smoker"), 
         `Basal characteristics` = str_to_lower(`Basal characteristics`))

# pivot_wider(arroyo_quiroz_HR, names_from = `Basal characteristics`, values_from = c(HR, lower, upper))

arroyo_quiroz_HR

write_csv(arroyo_quiroz_HR, here("Data", "arroyo_quiroz_HR_2020.csv"))


## OR for healthy ageing at age 77 (HA77) ----------------------------------
arroyo_quiroz_OR_HA77 <- arroyo_quiroz[16] 

# Extracicón de datos 
tab <- str_split(arroyo_quiroz_OR_HA77, "\n")
  tab <- tab[[1]]
  table <- c(tab[16:24], tab[30:36])
  table <- table[-7]

# Extracción del nombre de cada columna
names <- tab[15]
names <- names %>% 
    str_trim() %>% 
    str_split("\\s{2,}", simplify = T)
  names <- names[c(-2:-4, -6)]
  names <- matrix(names, nrow = 1, ncol = 3)

# Creación del data.frame
arroyo_quiroz_OR_HA77 <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>% 
  select(-"X2", -"X3", -"X4", -"X6") %>% 
  set_names(names) %>% 
  as_tibble()

arroyo_quiroz_OR_HA77 <- separate(data = arroyo_quiroz_OR_HA77, 
                                  col = `95% CI`, 
                                  into = c("lower", "upper"), sep = ",")

arroyo_quiroz_OR_HA77 <- arroyo_quiroz_OR_HA77 %>% 
  mutate(lower = str_remove(lower, pattern = "\\("), 
         upper = str_remove(upper, pattern = "\\)")) %>% 
  mutate_at(c("OR", "lower", "upper"), as.numeric) %>% 
  mutate(`Basal characteristics` = str_to_lower(`Basal characteristics`), 
         author_year = "Arroyo-Quiroz (2020)", 
         or_rr = "OR")

arroyo_quiroz_OR_HA77

write_csv(arroyo_quiroz_OR_HA77, here("Data", "arroyo_quiroz_OR_HA77_2020.csv"))


## OR for healthy ageing at age 90 (HA90)-s---------------------------------------------------------------
arroyo_quiroz_OR_HA90 <- arroyo_quiroz[17]

# Extracción de datos
tab <- str_split(arroyo_quiroz_OR_HA90, "\n")
  tab <- tab[[1]]
  table <- c(tab[16:25], tab[31:37])
  table <- table[-8]

# Extracción del nombre de cada columna 
names <- tab[15]
names
names <-  names %>% 
    str_trim() %>%
    str_split("\\s{2,}", simplify = T)
  names <- names[c(-2:-4, -6)]
  names <- matrix(names, nrow = 1, ncol = 3)

# Creación del data.frame
arroyo_quiroz_OR_HA90 <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>%  
  select(-"X2", -"X3", -"X4", -"X6") %>% 
  set_names(names) %>% 
  as_tibble()

arroyo_quiroz_OR_HA90 <- separate(data = arroyo_quiroz_OR_HA90, 
         col = `95% CI`, 
         into = c("lower", "upper"), sep = ",")

arroyo_quiroz_OR_HA90 <- arroyo_quiroz_OR_HA90 %>% 
  mutate(lower = str_remove(lower, pattern = "\\("),
         upper = str_remove(upper, pattern = "\\)"), 
         OR = str_remove(OR, pattern = "\\*")) %>% 
  mutate_at(c("OR", "lower", "upper"), as.numeric) %>% 
  dplyr::filter(!is.na(OR)) %>% 
  mutate(author_year = "Arroyo-Quiroz (2020)",
         or_rr = "OR",
         `Basal characteristics` = recode(`Basal characteristics`, 
                                          "Low-risk drinker" = "current drinker"), 
         `Basal characteristics` = str_to_lower(`Basal characteristics`))
  
arroyo_quiroz_OR_HA90

write_csv(arroyo_quiroz_OR_HA90, here("Data", "arroyo_quiroz_OR_HA90_2020.csv"))


### ------------------------------------------------------------------- ###
# Bell (2014) ---------------------------------------------------------------------------------
### ------------------------------------------------------------------- ###

# Extracción de datos 
# Extracción de nombres 
# Data.frame


### ------------------------------------------------------------------- ###
# Britton (2008) ------------------------------------------------------------------------------
### ------------------------------------------------------------------- ###
britton <- pdf_text(pdf = here("Articulos", "Britton (2008).pdf"))

per_hm <- 4140/5823*100
per_ee <- (548+246)/5823*100
per_t_disease <- (2549+988)/5823*100
per_t_function <- (757+361)/5823*100

## OR of Successful Aging ----------------------------------------------------------------------
britton_OR <- britton[4:5]

# Extracción de datos 
tab <- str_split(britton_OR, "\n")
tab <- c(tab[[1]], tab[[2]]) 
  # modelos generales 
  table <- c(tab[14:89])
  table <- table[c(-49:-63)]
  # modelos en cluster 
  table_2 <- tab[124:127]

# Extracción de nombres 
  # Se imputan manualmente. 
  
# Data.frame
###-------------###
# OR de ee
###-------------###
britton_OR <- table %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>% 
  as_tibble()

britton_OR <- britton_OR %>%
  filter(X2 != "" & str_detect(string = X3, pattern =  "\\(") == T) %>%
  subset(X1 != "P-value for trend" & X1 != "P-value")

britton_OR_temp <- britton_OR[1:2, 1:5] %>% select(-X2, -X4) %>% rename(X6 = X5) # primeros valores que se desacomodaron

  britton_OR_temp2 <- britton_OR %>% select(X1) 
  britton_OR_temp2$X2 <-  c(rep("Employment grade", 2), "Father Social class", rep("age left education", 2),      #
                          rep("height tertile", 2), rep("smoking", 2), rep("Alcohol, units/wk", 2), "Poor diet",  # etiqueta nombres
                          rep("Physical activity", 2), rep("Decision latitude", 2), rep("Job demands", 2),        #
                          rep("Work Support", 2), rep("Network index", 2))                                        #
  
  britton_OR_temp2 <- britton_OR_temp2 %>%
    mutate(X1 = recode(X1, "418" = ">18")) %>% 
    transmute(characteristics = str_c(X1, " (", X2, ")"), 
              X1 = 1)

############## Hombres 
# Aged ajusted 
britton_AOR_H <- 
  britton_OR[3:22,] %>% 
  select(X1, X3) %>% 
  bind_rows(britton_OR_temp, .) %>% 
  select(-X6) %>% 
  mutate(tmp_chunks = str_split(X3, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–"), n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(lower = str_remove(lower, "\\("), 
         upper = str_remove(upper, "\\)"), 
         characteristics = britton_OR_temp2$characteristics) %>% 
  select(characteristics, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_AOR_H, here("Data", "britton_AOR_H_2008.csv"))

# Age and SEP ajusted 
britton_AOR_SEP_H <- 
  britton_OR[3:22,] %>% 
  select(X1, X4) %>% 
  mutate(tmp_chunks = str_split(X4, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–"), n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(lower = str_remove(lower, "\\("), 
         upper = str_remove(upper, "\\)"), 
         characteristics = britton_OR_temp2$characteristics[3:22])  %>% 
  select(characteristics, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_AOR_SEP_H, here("Data", "britton_AOR_SEP_H_2008.csv"))

############  Mujeres
# Aged ajusted 
britton_AOR_M <- 
  britton_OR[3:22,] %>% 
  select(X1, X6) %>%  
  bind_rows(britton_OR_temp, .) %>%  
  select(-X3) %>% 
  mutate(tmp_chunks = str_split(X6, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–"), n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>%   
  mutate(lower = str_remove(lower, "\\("), 
         upper = str_remove(upper, "\\)"), 
         characteristics = britton_OR_temp2$characteristics) %>% 
  select(characteristics, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_AOR_M, here("Data", "britton_AOR_M_2008.csv"))

# Age and SEP ajusted 
britton_AOR_SEP_M <- 
  britton_OR[3:22,] %>% 
  select(X1, X7)  %>% 
  mutate(tmp_chunks = str_split(X7, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–"), n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(lower = str_remove(lower, "\\("), 
         upper = str_remove(upper, "\\)"), 
         characteristics = britton_OR_temp2$characteristics[3:22])  %>% 
  select(characteristics, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_AOR_SEP_M, here("Data", "britton_AOR_SEP_M_2008.csv"))


###-------------###
# cluster 
###-------------###
britton_OR_cluster <- table_2 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>% 
  as_tibble()

############## Hombres 
britton_OR_cluster_H <- 
  britton_OR_cluster %>% 
  select(X1, X2) %>% 
  mutate(tmp_chunks = str_split(X2, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–") , n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(upper = str_remove(upper, pattern = "\\)\\so\\.\\d{3}"), 
         lower = str_remove(lower, pattern = "\\(")) %>%
  select(X1, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>%
  rename(characteristics = X1) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_OR_cluster_H, here("Data", "britton_OR_cluster_H.csv"))

# Fully adjusted 
britton_OR_cluster_H_a <- 
  britton_OR_cluster %>% 
  select(X1, X3) %>% 
  mutate(tmp_chunks = str_split(X3, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–") , n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(upper = str_remove(upper, pattern = "\\)\\so\\.\\d{3}|\\)\\s\\.\\d{2,3}"), 
         lower = str_remove(lower, pattern = "\\(")) %>% 
  select(X1, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>%
  rename(characteristics = X1) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_OR_cluster_H_a, here("Data", "britton_OR_cluster_H_a.csv"))

############## Mujeres
britton_OR_cluster_M <- 
  britton_OR_cluster %>% 
  select(X1, X4) %>% 
  mutate(tmp_chunks = str_split(X4, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–") , n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(upper = str_remove(upper, pattern = "\\)\\so\\.\\d{3}|\\)\\s\\.\\d{2}"), 
         lower = str_remove(lower, pattern = "\\(")) %>% 
  select(X1, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>%
  rename(characteristics = X1) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_OR_cluster_M, here("Data", "britton_OR_cluster_M.csv"))

# Fully adjusted 
britton_OR_cluster_M_a <- 
  britton_OR_cluster %>% 
  select(X1, X5) %>% 
  mutate(tmp_chunks = str_split(X5, fixed(" "), n = 2)) %>% 
  mutate(AOR = map_chr(tmp_chunks, 1), 
         IC = map_chr(tmp_chunks, 2)) %>% 
  mutate(tmp_chunks2 = str_split(IC, fixed("–") , n = 2)) %>% 
  mutate(lower = map_chr(tmp_chunks2, 1),
         upper = map_chr(tmp_chunks2, 2)) %>% 
  mutate(upper = str_remove(upper, pattern = "\\)\\so\\.\\d{3}|\\)\\s\\.\\d{2,3}"), 
         lower = str_remove(lower, pattern = "\\(")) %>% 
  select(X1, AOR, lower, upper) %>% 
  mutate_at(c("AOR", "lower", "upper"), as.numeric) %>%
  rename(characteristics = X1) %>% 
  mutate(author_year = "Britton (2008)",
         or_rr = "OR",
         characteristics = str_to_lower(characteristics))

write_csv(britton_OR_cluster_M_a, here("Data", "britton_OR_cluster_M_a.csv"))
