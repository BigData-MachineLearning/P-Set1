# En este script vamos a importar la base de datos del Problem Set 1 de Big Data.

rm(list=ls())
library(pacman)
library(rvest)
library(dplyr)

p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skim, tidymodels, stargazer, broom)


# Limpio mi espacio de trabajo
rm(list = ls())

#Cargo los paquetes
require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skim, tidymodels, stargazer, broom)

tables <- list()


# Los datos de la pagina se sacan de url con un formato especifico, son tablas html
# Con formato de 

#https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/ + geih_page_ + (numero del 1-10)
# +.html)

for (i in 1:10) {
  #hay 10 paginas
  url_table <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/",
                                "geih_page_",i,".html"))
  #Guardo datos tabla 
  table <- html_table(url_table)
  tables[[i]] <- table
  
}

# Creo data frame con todo
geih <- as.data.frame(tables[[1]])
for (i in 2:10) {
  geih <- rbind(geih,as.data.frame(tables[[i]]))
}
# Ahora, dataframe_final contiene todos los datos de las 10 páginas en un solo dataframe

# Filtremos el DataFrame para seleccionar únicamenta aquellas personas ocupadas y cuya edad sea mayor o igual a 18. 

#Revisar obs unicas
nrow(unique(geih[c("directorio", "secuencia_p", "orden")]))

geih_clean <- geih %>% 
  filter(age>=18,ocu==1) %>% 
  select(directorio, secuencia_p, orden,age, p6050,  
         p6210, p7040, sex, estrato1, cotPension, 
         regSalud, cuentaPropia, formal, hoursWorkUsual,
         ocu,oficio, p6426,sizeFirm,  y_salary_m_hu, 
         iof1, iof2, iof6)%>%
  rename(wage=y_salary_m_hu,
         parentesco = p6050, 
         other_job = p7040,
         firm_time = p6426)
  mutate(age2=age*age, ln_salario=log(wage))

