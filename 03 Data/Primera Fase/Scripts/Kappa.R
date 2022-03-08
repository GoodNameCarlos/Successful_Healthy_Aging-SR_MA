
# Cohen Kappa -------------------------------------------------------------

install.packages("irr")
library(irr)
library(psych)

## Sample size: Kappa ------------------------------------------------------

# Debemos iniciar definiendo el nivel de Kappa que sería nuestra H0. Puede ser que  
# sea de 0.4 en el límite superior, de lo que sería inaceptablemente bajo. Por lo que
# tenemos que definir qué sería el nivel de concordancia aceptable. 
# Puede ser que sea un nivel de 0.75 el valor bajo, lo que sería considerado concordancia "substancial"
# Y finalmente, se tiene que tener una idea de la prevalencia del la característica a medir. 

# ejemplo, podemos querer probar una H0 de kappa == 0.75, e HA == > 0.75. 
# donde kappa sea == 0.90 y los dos evaluadores puedan classificar al 10% de los componentes como positivos. 

N.cohen.kappa(0.30, 0.30, k1 = 0.85, k0 = .75)

## Estadística de Kappa ----------------------------------------------------


