library(tidyverse)
library(here)
library(pdftools)
# Britton supplementary -----------------------------------------------------------------------


britton_supp <- pdf_text(pdf = here("Articulos", "Britton_supp (2008).pdf"))  
britton_supp


# Disease free ----------------------------------------------------------------------------------------------------
disease_free <- britton_supp[1]
disease_free <- str_split(disease_free, "\n") 
disease_free <- disease_free[[1]]
disease_free_1 <- disease_free[11:12]
disease_free_2 <- disease_free[c(16:20, 27:62)]

disease_free_2 <- str_remove(string = disease_free_2, pattern = ".*-")

disease_free_2 <- disease_free_2 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F)  %>% 
  filter(X1 != "value" & X1 != "value for trend" & X2 != "" & X3 != "1.0") %>% 
  select(X1, X3, X6)

###---arriba---###
disease_free_1 <- disease_free_1 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = T) %>% 
  rename(X6 = X5) %>% 
  select(X1, X3, X6) 

disease_free_1$X1 <- str_remove(disease_free_1$X1, pattern = ".*-")

###---pegado---###

disease_free <- bind_rows(disease_free_1, disease_free_2)

###---nombres---###

disease_free$X2 <- c(rep("Employment grade", 2), "Father Social class", rep("age left education", 2),      
  rep("height tertile", 2), rep("smoking", 2), rep("Alcohol, units/wk", 2), "Poor diet",
  rep("Physical activity", 2), rep("Decision latitude", 2), rep("Job demands", 2),
  rep("Work Support", 2), rep("Network index", 2))

disease_free <- disease_free %>% 
   mutate(X1 = recode(X1, 
                      "18 yrs" = "17-18 yrs", 
                      "14" = "1-14")) %>% 
   mutate(characteristics = str_c(X1," (", X2, ")")) %>% 
   select(-X1, -X2) %>% 
   relocate(characteristics, .before = X3) 
 
###---OR---###
disease_free <- disease_free %>% 
  mutate(tmp_chunk1 = str_split(X3, fixed(" "), n = 3), 
         tmp_chunk2 = str_split(X6, fixed(" "), n = 3)) %>% 
  mutate(OR_h = map_chr(tmp_chunk1, 1), 
         lower_h = map_chr(tmp_chunk1, 2), 
         upper_h = map_chr(tmp_chunk1, 3), 
         OR_m = map_chr(tmp_chunk2, 1),
         lower_m = map_chr(tmp_chunk2, 2),
         upper_m = map_chr(tmp_chunk2, 3)) %>% 
  mutate(lower_h = str_remove(lower_h, "\\("), 
         upper_h = str_remove(upper_h, "\\)"), 
         lower_m = str_remove(lower_m, "\\("), 
         upper_m = str_remove(upper_m, "\\)")) %>% 
  mutate(lower_h = str_remove(lower_h, "\\,"),
         lower_m = str_remove(lower_m, "\\,")) %>% 
  select(characteristics, OR_h, lower_h, upper_h, OR_m, lower_m,  upper_m) 

disease_free <- as_tibble(disease_free)

disease_free <- disease_free %>% 
  mutate_at(c("OR_h", "lower_h", "upper_h", "OR_m", "lower_m", "upper_m"), as.numeric)



# Good functioning ------------------------------------------------------------------------------------------------

# Disease free
good_function <- britton_supp[2]
good_function <- str_split(good_function, "\n") 
good_function <- good_function[[1]]
good_function_1 <- good_function[10:11]
good_function_2 <- good_function[c(15:19, 27:61)]

good_function_2 <- str_remove(string = good_function_2, pattern = ".*-")

good_function_2 <- good_function_2 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F)  %>% 
  filter(X1 != "value" & X1 != "value for trend" & X2 != "" & X3 != "1.0") %>% 
  select(X1, X3, X6)

###---arriba---###
good_function_1 <- good_function_1 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = T) %>% 
  rename(X6 = X5) %>% 
  select(X1, X3, X6) 

good_function_1$X1 <- str_remove(good_function_1$X1, pattern = ".*-")

###---pegado---###

good_function <- bind_rows(good_function_1, good_function_2)

###---nombres---###

good_function$X2 <- c(rep("Employment grade", 2), "Father Social class", rep("age left education", 2),      
                     rep("height tertile", 2), rep("smoking", 2), rep("Alcohol, units/wk", 2), "Poor diet",
                     rep("Physical activity", 2), rep("Decision latitude", 2), rep("Job demands", 2),
                     rep("Work Support", 2), rep("Network index", 2))

good_function <- good_function %>% 
  mutate(X1 = recode(X1, 
                     "18 yrs" = "17-18 yrs", 
                     "14" = "1-14")) %>% 
  mutate(characteristics = str_c(X1," (", X2, ")")) %>% 
  select(-X1, -X2) %>% 
  relocate(characteristics, .before = X3) 

###---OR---###
good_function <- good_function %>% 
  mutate(tmp_chunk1 = str_split(X3, fixed(" "), n = 3), 
         tmp_chunk2 = str_split(X6, fixed(" "), n = 3)) %>% 
  mutate(OR_h = map_chr(tmp_chunk1, 1), 
         lower_h = map_chr(tmp_chunk1, 2), 
         upper_h = map_chr(tmp_chunk1, 3), 
         OR_m = map_chr(tmp_chunk2, 1),
         lower_m = map_chr(tmp_chunk2, 2),
         upper_m = map_chr(tmp_chunk2, 3)) %>% 
  mutate(lower_h = str_remove(lower_h, "\\("), 
         upper_h = str_remove(upper_h, "\\)"), 
         lower_m = str_remove(lower_m, "\\("), 
         upper_m = str_remove(upper_m, "\\)")) %>% 
  mutate(lower_h = str_remove(lower_h, "\\,"),
         lower_m = str_remove(lower_m, "\\,")) %>% 
  select(characteristics, OR_h, lower_h, upper_h, OR_m, lower_m,  upper_m) 

good_function <- as_tibble(good_function)

good_function <- good_function %>% 
  mutate_at(c("OR_h", "lower_h", "upper_h", "OR_m", "lower_m", "upper_m"), as.numeric) 

good_function$characteristics <- factor(good_function$characteristics, levels = good_function$characteristics) 
 
 

# Plots -----------------------------------------------------------------------------------------------------------

good_function %>% 
  ggplot(aes(OR_h, characteristics)) +
  geom_pointrange(aes(xmin = lower_h, xmax = upper_h)) +
  scale_y_discrete(limits = rev(levels(good_function$characteristics))) +
  
  
  
  
  

  


