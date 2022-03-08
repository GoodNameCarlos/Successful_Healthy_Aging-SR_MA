library(pdftools)
library(here)

arroyo_quiroz <- pdf_text(pdf = here("Arts", "Arroyo-Quiroz (2020).pdf"))

arroyo_quiroz_OR_HA90 <- arroyo_quiroz[17]
  
# Extracción de datos
tab <- str_split(arroyo_quiroz_OR_HA90, "\n")
  tab <- tab[[1]]
  table <- c(tab[16:25], tab[31:37])
  table_l <- table[-8:-9]

# Extracción del nombre de cada columna 
names <- tab[15]
names
names <-  names %>% 
    str_trim() %>%
    str_split("\\s{2,}", simplify = T)
  names <- names[-2:-4]
  names <- matrix(names, nrow = 1, ncol = 4)

# Creación del data.frame
arroyo_quiroz_OR_HA90 <- table_l %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F) %>% 
  select(-"X2", -"X3", -"X4") %>% 
  set_names(names) %>% 
  as_tibble()

arroyo_quiroz_OR_HA90 %>% 
  select(-`P- value`)
  

arroyo_quiroz_OR_HA90 <- separate(data = arroyo_quiroz_OR_HA90, 
         col = `95% CI`, 
         into = c("lower", "upper"), sep = ",")

arroyo_quiroz_OR_HA90$lower <- str_remove(string = arroyo_quiroz_OR_HA90$lower, pattern = "\\(")
arroyo_quiroz_OR_HA90$upper <- str_remove(string = arroyo_quiroz_OR_HA90$upper, pattern = "\\)")

arroyo_quiroz_OR_HA90




