
# Dimensiones y sub dimensiones -------------------------------------------

###------------------------------------------------------------------------------------------------------------------###
# • Health factors: 
  # Physical function (e.g., ADL, IADLS, Disability),
  sub_health_physical <- c("adl|iadl|adelaide|stand|walk|grip")
  
  # Cognitive function (e.g., cognitive preserved),
  sub_health_cognitive <- c("memory|speed|naming|nart|similarities")
  
  # Physiological health (e.g., metabolic health, presence of chronic disease, Blood pressure, BMI).
  sub_health_physio <- c("weight|obesity|sleep|snores")
  
  # General health status (e.g., self-rated health, self-rated successful aging)
  sub_health_general <- c("self-rated health")
  
dom_health <- c(sub_health_physical, "|", sub_health_cognitive, "|", sub_health_physio)

###------------------------------------------------------------------------------------------------------------------###
# • Psychological Factors: 
  # Mental health (e.g., presence of depression symptoms),
  sub_psych_mental <- c("depression")

  # Psychological well-being (e.g., quality of life, goal pursuit, personal resources).
  sub_psych_well_being <- c("morale|self-esteem|control")

dom_psych <- str_c(sub_psych_mental, "|", sub_psych_well_being)

###------------------------------------------------------------------------------------------------------------------###
# • Behavioral factors: Health behaviors (e.g., physical activities, smoking status, alcohol consumption, diet).
  sub_beha_health <- c("smoker|drinker|exercise")

dom_behavioral <- sub_beha_health

###------------------------------------------------------------------------------------------------------------------###
# • Social Factors: 
  # Social well-being (e.g., social participation, marital status), 
  sub_soc_well_being <- c("married")

  # Socio demographic (e.g., Age, socioeconomic status, education level), 
  sub_soc_demo <- c("age|gender|male|female|schooling")
  
  # Environmental factors (e.g., access to public spaces, environmental security).
  sub_soc_enviro <- c("")

dom_social <- str_c(sub_soc_well_being, "|", sub_soc_demo, "|", sub_soc_enviro)

###------------------------------------------------------------------------------------------------------------------###
# • Other factors: Health indices or short form surveys.
other <- c("mother|father|follow-up")
index <- c()

dom_other <- str_c(other, "|", index)



# NOTAS -----------------------------------------------------------------------------------------------------------

# Se codifican las bases de datos para que toda palabra quede con la primera letra en mayuscula. Se puede arreglar en el
# análisis el formato de la presentación de los resultados.

# ♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪♫♪

# Ejemplo del manejo de las transformaciones

arroyo_quiroz_OR_HA90 %>% 
  mutate(loco = case_when(str_detect(`Basal characteristics`, sub_health_physio) == T ~ 1, 
                          str_detect(`Basal characteristics`, sub_psych_mental) == T ~ 2,
                          str_detect(`Basal characteristics`, sub_beha_health) == T ~3,
                          str_detect(`Basal characteristics`, sub_soc_demo) == T ~ 4,
                          str_detect(`Basal characteristics`, other) == T ~ 5, 
                          T ~ 6)) 
