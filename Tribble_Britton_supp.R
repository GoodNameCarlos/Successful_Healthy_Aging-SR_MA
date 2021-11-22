library(tidyverse)
library(here)
library(pdftools)
# install.packages("gridExtra")
library(gridExtra)
# install.packages("cowplot")
library(cowplot)
# Britton supplementary -----------------------------------------------------------------------
britton_supp <- pdf_text(pdf = here("Articulos", "Britton_supp (2008).pdf"))  

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


disease_free$characteristics <- factor(disease_free$characteristics, levels = disease_free$characteristics) 

# Good functioning ------------------------------------------------------------------------------------------------
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

# Disease free (No metabolic) -------------------------------------------------------------------------------------
disease_free_noM <- britton_supp[3]
disease_free_noM <- str_split(disease_free_noM, "\n") 
disease_free_noM <- disease_free_noM[[1]]
disease_free_noM_1 <- disease_free_noM[10:11]
disease_free_noM_2 <- disease_free_noM[c(15:19, 27:62)]

disease_free_noM_2 <- str_remove(string = disease_free_noM_2, pattern = ".*-")

disease_free_noM_2 <- disease_free_noM_2 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F)  %>% 
  filter(X1 != "value" & X1 != "value for trend" & X2 != "" & X3 != "1.0") %>% 
  select(X1, X3, X6)

###---arriba---###
disease_free_noM_1 <- disease_free_noM_1 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = T) %>% 
  rename(X6 = X5) %>% 
  select(X1, X3, X6) 

disease_free_noM_1$X1 <- str_remove(disease_free_noM_1$X1, pattern = ".*-")

###---pegado---###

disease_free_noM <- bind_rows(disease_free_noM_1, disease_free_noM_2)

###---nombres---###

disease_free_noM$X2 <- c(rep("Employment grade", 2), "Father Social class", rep("age left education", 2),      
                     rep("height tertile", 2), rep("smoking", 2), rep("Alcohol, units/wk", 2), "Poor diet",
                     rep("Physical activity", 2), rep("Decision latitude", 2), rep("Job demands", 2),
                     rep("Work Support", 2), rep("Network index", 2))

disease_free_noM <- disease_free_noM %>% 
  mutate(X1 = recode(X1, 
                     "18 yrs" = "17-18 yrs", 
                     "14" = "1-14")) %>% 
  mutate(characteristics = str_c(X1," (", X2, ")")) %>% 
  select(-X1, -X2) %>% 
  relocate(characteristics, .before = X3) 

###---OR---###
disease_free_noM <- disease_free_noM %>% 
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

disease_free_noM <- as_tibble(disease_free_noM)

disease_free_noM <- disease_free_noM %>% 
  mutate_at(c("OR_h", "lower_h", "upper_h", "OR_m", "lower_m", "upper_m"), as.numeric)

disease_free_noM$characteristics <- factor(disease_free_noM$characteristics, levels = disease_free_noM$characteristics) 

# Good functioning (No Cognition) ---------------------------------------------------------------------------------
good_function_nocog <- britton_supp[4]
good_function_nocog <- str_split(good_function_nocog, "\n") 
good_function_nocog <- good_function_nocog[[1]]
good_function_nocog_1 <- good_function_nocog[12:13]
good_function_nocog_2 <- good_function_nocog[c(17:21, 27:64)]

good_function_nocog_2 <- str_remove(string = good_function_nocog_2, pattern = ".*-")

good_function_nocog_2 <- good_function_nocog_2 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = F)  %>% 
  filter(X1 != "value" & X1 != "value for trend" & X2 != "" & X3 != "1.0") %>% 
  select(X1, X3, X6)

###---arriba---###
good_function_nocog_1 <- good_function_nocog_1 %>% 
  str_trim() %>% 
  str_split("\\s{2,}", simplify = T) %>% 
  data.frame(stringsAsFactors = T) %>% 
  rename(X6 = X5) %>% 
  select(X1, X3, X6) 

good_function_nocog_1$X1 <- str_remove(good_function_nocog_1$X1, pattern = ".*-")

###---pegado---###

good_function_nocog <- bind_rows(good_function_nocog_1, good_function_nocog_2)

###---nombres---###

good_function_nocog$X2 <- c(rep("Employment grade", 2), "Father Social class", rep("age left education", 2),      
                      rep("height tertile", 2), rep("smoking", 2), rep("Alcohol, units/wk", 2), "Poor diet",
                      rep("Physical activity", 2), rep("Decision latitude", 2), rep("Job demands", 2),
                      rep("Work Support", 2), rep("Network index", 2))

good_function_nocog <- good_function_nocog %>% 
  mutate(X1 = recode(X1, 
                     "18 yrs" = "17-18 yrs", 
                     "14" = "1-14")) %>% 
  mutate(characteristics = str_c(X1," (", X2, ")")) %>% 
  select(-X1, -X2) %>% 
  relocate(characteristics, .before = X3) 

###---OR---###
good_function_nocog <- good_function_nocog %>% 
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

good_function_nocog <- as_tibble(good_function_nocog)

good_function_nocog <- good_function_nocog %>% 
  mutate_at(c("OR_h", "lower_h", "upper_h", "OR_m", "lower_m", "upper_m"), as.numeric) 

good_function_nocog$characteristics <- factor(good_function_nocog$characteristics, levels = good_function_nocog$characteristics) 





# Plots -----------------------------------------------------------------------------------------------------------

disease_free_G <- disease_free %>% 
  ggplot(aes(OR_h, characteristics)) +
  geom_pointrange(aes(xmin = lower_h, xmax = upper_h), size = .5) +
  scale_y_discrete(limits = rev(levels(disease_free$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "") +
  ylab("") +
  theme_bw() +
  xlim(0,12)  +
  labs(title = "Staying disease free") 
  

good_function_G <- good_function %>%
  ggplot(aes(OR_h, characteristics)) +
  geom_pointrange(aes(xmin = lower_h, xmax = upper_h), size = .5) +
  scale_y_discrete(limits = rev(levels(good_function$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "") +
  ylab("") +
  theme_bw() +
  xlim(0,12) + 
  geom_segment(aes(x = 12, y = 22, xend = 5.3, yend = 22)) +
  geom_text(x = 11, y = 21.5, label = "IC superior >15", col = "darkred", size = 2) +
  labs(title = "Good functioning")

disease_free_noM_G <- disease_free_noM %>% 
  ggplot(aes(OR_h, characteristics)) +
  geom_pointrange(aes(xmin = lower_h, xmax = upper_h), size = .5) +
  scale_y_discrete(limits = rev(levels(disease_free_noM$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "Odds ratio") +
  theme_bw() +
  xlim(0,12)+
  geom_segment(aes(x = 12, y = 22, xend = 4.1, yend = 22)) +
  geom_text(x = 11, y = 21.5, label = "IC superior >15", col = "darkred", size = 2) +
  labs(title = "No metabolic disease") +
  theme(axis.title.y =element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())  

good_function_nocog_G <- good_function_nocog %>%
  ggplot(aes(OR_h, characteristics)) +
  geom_pointrange(aes(xmin = lower_h, xmax = upper_h), size = .5) +
  scale_y_discrete(limits = rev(levels(good_function_nocog$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "Odds ratios") +
  theme_bw() +
  xlim(0,12) +
  labs(title = "Excluding cognitive functioning") +
  theme(axis.title.y =element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())

grid.arrange(disease_free_G, disease_free_noM_G, good_function_G, good_function_nocog_G,nrow = 2, ncol = 2, )

plot_grid(disease_free_G, disease_free_noM_G, good_function_G, good_function_nocog_G,
          ncol = 2,
          nrow = 2,
          align = "v")

ggsave(filename = "plot H.jpg", path = here("Figs"), plot = grid_m)

### --- mujeres --- ###

disease_free_G_m <- disease_free %>% 
  ggplot(aes(OR_m, characteristics)) +
  geom_pointrange(aes(xmin = lower_m, xmax = upper_m), size = .5) +
  scale_y_discrete(limits = rev(levels(disease_free$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "") +
  ylab("") +
  theme_bw() +
  xlim(0,12)  +
  labs(title = "Staying disease free") 


good_function_G_m <- good_function %>%
  ggplot(aes(OR_m, characteristics)) +
  geom_pointrange(aes(xmin = lower_m, xmax = upper_m), size = .5) +
  scale_y_discrete(limits = rev(levels(good_function$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "") +
  ylab("") +
  theme_bw() +
  xlim(0,12) + 
  geom_segment(aes(x = 12, y = 22, xend = 5.3, yend = 22)) +
  geom_text(x = 11, y = 21.5, label = "IC superior >15", col = "darkred", size = 2) +
  labs(title = "Good functioning")

disease_free_noM_m <- disease_free_noM %>% 
  ggplot(aes(OR_m, characteristics)) +
  geom_pointrange(aes(xmin = lower_m, xmax = upper_m), size = .5) +
  scale_y_discrete(limits = rev(levels(disease_free_noM$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "Odds ratio") +
  theme_bw() +
  xlim(0,12)+
  geom_segment(aes(x = 12, y = 22, xend = 4.1, yend = 22)) +
  geom_text(x = 11, y = 21.5, label = "IC superior >15", col = "darkred", size = 2) +
  labs(title = "No metabolic disease") +
  theme(axis.title.y =element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())  

good_function_nocog_G_m <- good_function_nocog %>%
  ggplot(aes(OR_m, characteristics)) +
  geom_pointrange(aes(xmin = lower_m, xmax = upper_m), size = .5) +
  scale_y_discrete(limits = rev(levels(good_function_nocog$characteristics))) +
  geom_vline(xintercept = 1, col = "blue", size = 1) +
  xlab(label = "Odds ratios") +
  theme_bw() +
  xlim(0,12) +
  labs(title = "Excluding cognitive functioning") +
  theme(axis.title.y =element_blank(),
        axis.text.y =element_blank(),
        axis.ticks.y =element_blank())


plot_grid(disease_free_G_m, disease_free_noM_G_m, good_function_G_m, good_function_nocog_G_m,
          ncol = 2,
          nrow = 2,
          align = "v")
