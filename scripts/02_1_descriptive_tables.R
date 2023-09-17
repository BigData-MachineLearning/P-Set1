### Punto 2 ### primera reg

# A descriptive analysis of the data. At a minimum, you should include a descrip-
# tive statistics table with its interpretation. However, I expect a deep analysis
# that helps the reader understand the data, its variation, and the justification for
# your data choices. 

# Use your professional knowledge to add value to this section.
# Do not present it as a “dry” list of ingredients


geih2018 <- read_csv("stores/geih_clean.csv")

colnames(geih2018)

geih2018 <- as.data.frame(geih2018)
geih2018 <- geih2018 |>
  mutate(lwage = log(wage))

require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, boot)

# agrupar y describir

# Tabla con toda la lista

# Tabla ingresos ( wages, ofi)
ingresos <- geih2018[c("wage", "iof1", "iof2", "iof6")]

stargazer(as.data.frame(ingresos), 
          type = "text",title="Descriptive statistics - Variables ingresos", 
          digits=1, out="table2.txt",
          covariate.labels=c("salary - real hourly (usual)", 
                             "Intereses y dividendos por inversiones", 
                             "Pensiones y jubilaciones", "Arriendos"))

# Tabla sociodemo (age, sex,education)

sociodem <- geih2018[c("age", "p6210", "sex", "estrato1")]

stargazer(as.data.frame(sociodem), 
          type = "text", title="Descriptive statistics Variables sociodemográficas",
          digits=1, out="table2.txt",
          covariate.labels=c("edad", "educacion", 
                             "sexo", "estrato"))

# Tabla labor (cuenta propia, pension, salud)

laboral <- geih2018[c("cotPension", "regSalud", "cuentaPropia", "formal", "hoursWorkUsual",
                      "oficio",  "firm_time",  "sizeFirm",  "other_job")]

stargazer(as.data.frame(laboral),
          type = "text",  title="Descriptive statistics Variables laborales", 
          digits=1, out="table2.txt",
          covariate.labels=c("pension", "salud", "cuenta propia", "formal", 
                             "horas usual", "oficio",  "Tiempo en empresa", 
                             "tamaño firma", " other job" ))