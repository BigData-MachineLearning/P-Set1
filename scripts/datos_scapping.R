# En este script vamos a importar la base de datos del Problem Set 1 de Big Data.

rm(list=ls())
library(pacman)
library(rvest)
library(dplyr)

p_load(rvest,tidyverse,kableExtra,readxl)

url <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/'

html <- read_html(url)

#Saquemos primero los labels:

url_labels <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html'
html_labels <- read_html(url_labels)


#Veamos cuáles son las descripciones de la las variables:

url_dict <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html'
html_dict <- read_html(url_dict)
tabla_dict <- html_dict %>% html_table()%>% as.data.frame()

#Ahora si hagamos el Scrapping: 

# Inicializamos un dataframe vacío para almacenar los datos
GEIH <- data.frame()

# Itera a través de las 10 páginas

for (i in 1:10) {
  # Contruimos el URL que va a iterar:
  link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  
  # Leemos la página WEB:
  html_link <- read_html(link)
  
  # Raspamos la tabla de la página y la convertimos en un dataframe:
  tabla <- html_link %>% html_table() %>% as.data.frame()
  
  # Agrega los datos al dataframe final
  GEIH <- bind_rows(dataframe_final, tabla)
}

# Ahora, dataframe_final contiene todos los datos de las 10 páginas en un solo dataframe

# Filtremos el DataFrame para seleccionar únicamenta aquellas personas ocupadas y cuya edad sea mayor o igual a 18. 

GEIH_filtrado <- GEIH %>% 
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

