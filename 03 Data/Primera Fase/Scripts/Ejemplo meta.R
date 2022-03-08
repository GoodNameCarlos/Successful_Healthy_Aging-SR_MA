# install.packages("metasens")

library(meta)
library(questionr)
library(metasens)


# Ejemplo con easymeta -----------------------------------------------------

Meta_ex <- tribble(
  ~trial, ~year, ~OR, ~"OR Low", ~"OR Up", ~group, ~x, 
"Lorenzo", 2001, 1.57, 1.23, 2.00, "A", 0.517,
"Constanza", 2002, 2.03, 1.31, 3.15, "B", -1.722,
"Anne-Marie", 2004, 1.92, 1.51, 2.44, "B", -1.015,
"Elisabeth", 2008, 1.55, 1.22, 1.97, "B", -0.239,
"Simon", 2011, 1.25, 0.77, 2.02, "A", 1.042,
"Neil", 2012, 1.12, 0.72, 1.73, "A", 2.026,
"Milena", 2014, 1.64, 1.24, 2.16, "B", -0.729,
"Stefania", 2018, 2.10, 1.69, 2.60, "B", -1.812,
"Manolis", 2021, 1.40, 1.06, 1.85, "A", 0.862,
"Aurelio", 2022, 1.62, 1.33, 1.97, "A", 0.298)

m.gen_bin <- metagen(TE = OR, 
                     lower = `OR Low`, 
                     upper = `OR Up`, 
                     studlab = trial, 
                     data = Meta_ex, 
                     sm = "OR", 
                     fixed = F,
                     random = T)

forest(m.gen_bin, prediction = T) 
funnel(m.gen_bin, ylim = c(0.6, 0))

Bueno <- metagen(log(OR), lower = log(`OR Low`), upper = log(`OR Up`), studlab = str_c(Meta_ex$trial, as.character(Meta_ex$year), sep = " "), data = Meta_ex, sm = "OR", prediction = T)

forest(Bueno)
funnel(Bueno)

Bueno2 <- metagen(log(OR), lower = log(`OR Low`), upper = log(`OR Up`),
                 studlab = str_c(Meta_ex$trial, as.character(Meta_ex$year), sep = " "), 
                 data = Meta_ex, sm = "OR", 
                 subgroup = group)

forest(Bueno2, fixed = F, random = T, prediction = T)

      # Ejemplo de metagen con HR 
          study <- c("FCG on CLL 1996", "Leporrier 2001", "Rai 2000", "Robak 2000")
          HR <- c(0.55, 0.92, 0.79, 1.18)
          lower.HR <- c(0.28, 0.79, 0.59, 0.64)
          upper.HR <- c(1.09, 1.08, 1.05, 2.17)
          
          a <- metagen(log(HR), lower = log(lower.HR), upper = log(upper.HR),
                       studlab = study, sm = "HR")
          forest(a)

# Ejercicio del articulo Balduzzy (2019) ----------------------------------

settings.meta(digits = 2)

(joy <- tribble(
~author, ~year,~resp.h, ~fail.h, ~drop.h, ~resp.p, ~fail.p, ~drop.p,
"Arvanitis"     ,1997,25,25,2,18,33,0, 
"Beasley"       ,1996,29,18,22,20,14,34, 
"Bechelli"      ,1983,12,17,1,2,28,1, 
"Borison"       ,1992,3,9,0,0,12,0, 
"Chouinard"     ,1993,10,11,0,3,19,0, 
"Durost"        ,1964,11,8,0,1,14,0, 
"Garry"         ,1962,7,18,1,4,21,1, 
"Howard"        ,1974,8,9,0,3,10,0, 
"Marder"        ,1994,19,45,2,14,50,2, 
"Nishikawa"     ,1982,1,9,0,0,10,0, 
"Nishikawa"     ,1984,11,23,3,0,13,0, 
"Reschke"       ,1974,20,9,0,2,9,0, 
"Selman"        ,1976,17,1,11,7,4,18, 
"Serafetinides" ,1972,4,10,0,0,13,1, 
"Simpson"       ,1967,2,14,0,0,7,1, 
"Spencer"       ,1992,11,1,0,1,11,0, 
"Vichaiya"      ,1971,9,20,1,0,29,1))

# Generamos una variable con la información de si el estudio contiene missngs, esto a partir de 
# que las variables "drop.h" y "drop.h" tengan algun valor. 

joy$miss = ifelse((joy$drop.h + joy$drop.p) == 0, 
                  c("Without missing data"), c("With missing data"))

freq(joy$miss)

# El evento de interes, que es la mejora clínica, es binaria por lo que el análisis necesaria es metabin

help(meta) # decidimos metabin (meta binario) ya que la documentación indica que es la función idicada para este tipo de data. 

# Generamos una lista con la funcion "Metabin" que contiene componentes describiendo el metaanalisis.  
# como default establece la razon de riesgos (RR) como efecto de medición. 

m.publ <- metabin(resp.h, resp.h + fail.h, resp.p, resp.p + fail.p, # los primeros 4 argumentos definen la variable con el número de pacientes que experimentan una mejora clínica y el total de todos los pacientes aleatorizados, controles y experimentales. 
                  data = joy, # data de donde obtenemos la información 
                  studlab = paste0(author, " (", year, ")"), # Identificadores de los estudios. Se junta la información de autores y año, se pone en paréntesis el año. 
                  method.tau = "PM") # Se cambia el modelo de análisis por efectos aleatorios de Paule & Mandel  
                                     # El método de análisis por default para calcular el efecto fijo es Mantel & Haenszel. 

m.publ

# Graficamos el forest plot del análisis. 

forest(m.publ, sortvar = year, prediction = T, label.left = "Favours Placebo", label.right = "Favours haloperidol")

# Evaluando el impacto de los datos missing
# para entender si los resultados de los estudios con missings difieren de aquellos con datos completos. 
# Se puede hacer una analisis por subgrupos. 

# actualizamos la lista "m.publ" 

m.publ.sub <- update(m.publ, subgroup = miss, print.subgroup.name =  F)

  forest(m.publ.sub, prediction = T, label.left = "Favours Placebo", label.right = "Favours haloperidol") 

  funnel(m.publ, contour.levels = c(0.9, 0.95,0.99), col.contour = c ("darkgray", "gray", "lightgray"))
  
  
mmiss.1 = metamiss(m.publ, drop.h, drop.p, method.miss = "1")
