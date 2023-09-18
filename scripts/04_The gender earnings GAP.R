# Problem Set 1 - Big data - Machine Learning: 

# 4. The gender earnings GAP:

# Policymakers have long been concerned with the gender
# wage gap, and is going to be our focus in this subsection.

# paquetes

require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, boot)

# Importar datos: 
geih_clean <- readRDS("C:/Users/danie/Documents/Universidad/8vo Semestre economía/Big Data y Machine Learning/Problem set 1/Datos/geih_clean.rds")

geih_clean <- geih_clean %>%
  mutate(lwage = log(wage),
         female = case_when(sex == 1 ~ 0,
                            sex == 0 ~ 1))
view(geih_clean)

# a) Begin by estimating and discussing the unconditional wage gap:

# log(w) = B1 + B2Female + u

# where Female is an indicator that takes one if the individual in the sample is
# identified as female.

geih_clean <- geih_clean %>%
  mutate(lwage = log(wage))

modelo_2 <- lm(lwage ~ female, data = geih_clean)

stargazer(modelo_2,
          type = "text",title = "Unconditional wage gap") ## B2 = -0.045**, var = 0.015

#(b) Equal Pay for Equal Work? A common slogan is“equal pay for equal work”. One
#way to interpret this is that for employees with similar worker and job characteristics,
#no gender wage gap should exist. Estimate a conditional earnings gap
#incorporating control variables such as similar worker and job characteristics.
#In this section, estimate the conditional wage gap:

## Datos sin missing values: 

geih_final <- readRDS("C:/Users/danie/Documents/Universidad/8vo Semestre economía/Big Data y Machine Learning/Problem set 1/Datos/geih_final.rds")
geih_final <- geih_final %>%
  mutate(female = case_when(sex == 1 ~ 0,
                            sex == 0 ~ 1))
view(geih_final)

# 1. Only FWL

# Regresion Female - controles: 

modelo_3 <- lm(female ~ poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6, data = geih_final)

# Residuales_mod_3:

residuales_mod_3 <- resid(modelo_3)

# Regresion lwage - controles: 

modelo_4 <-lm(lwage ~poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                estrato1 + cotPension + regSalud + cuentaPropia + formal +
                poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                sizeFirm + iof1 + iof2 + iof6, data = geih_final)

# Residuales_mod_3:
residuales_mod_4 <- resid(modelo_4)

# Regresion residuales: 

modelo_5 <- lm(residuales_mod_4 ~ residuales_mod_3, data = geih_final)

stargazer(modelo_5, type = "text")


# 2. FWL - Boostrap



