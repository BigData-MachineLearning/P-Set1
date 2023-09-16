# En este script vamos a importar la base de datos del Problem Set 1 de Big Data.
rm(list=ls())

library(pacman)
p_load(rvest,tidyverse,kableExtra,readxl)
library(rvest)
library(dplyr)

url <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/'

html <- read_html(url)

#Saquemos primero los labels:

url_labels <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/labels.html'
html_labels <- read_html(url_labels)

tablas <- html_labels %>% html_table()
labels <- tablas[[1]]

#Veamos cuáles son las descripciones de la las variables:

url_dict <- 'https://ignaciomsarmiento.github.io/GEIH2018_sample/dictionary.html'
html_dict <- read_html(url_dict)
tabla_dict <- html_dict %>% html_table()%>% as.data.frame()

#Con la clase, sabemos que la información que queremos está en este link:

# Inicializa un dataframe vacío para almacenar los datos
dataframe_final <- data.frame()

# Itera a través de las 10 páginas
for (i in 1:10) {
  # Construye la URL de la página actual
  link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  
  # Lee la página web
  html_link <- read_html(link)
  
  # Raspa la tabla de la página y conviértela en un dataframe
  tabla <- html_link %>% html_table() %>% as.data.frame()
  
  # Agrega los datos al dataframe final
  dataframe_final <- bind_rows(dataframe_final, tabla)
}

# Ahora, dataframe_final contiene todos los datos de las 10 páginas en un solo dataframe}
names(dataframe_final)

# Filtremos el DataFrame para seleccionar únicamenta aquellas personas ocupadas y cuya edad sea mayor o igual a 18. 

data_filtrado <- dataframe_final %>% 
  filter(age>=18,ocu==1) %>% 
  select("age","ocu", "maxEducLevel", "sex", "formal", "p6210s1", "wap", "y_vivienda_m", "p6426","clase", "hoursWorkUsual",
         "p6050", "p6585s1", "p7040", "y_salary_m_hu", "college", "cuentaPropia", "estrato1", "y_gananciaIndep_m","formal",
         "cotPension", "relab", "y_bonificaciones_m", "y_auxilioAliment_m", "y_auxilioTransp_m", "ingtot","fweight",
         "totalHoursWorked", "cuentaPropia", "estrato1")%>%
  mutate(age2=age*age, ln_salario=log(y_salary_m_hu))%>%
  rename(wage_h=y_salary_m_hu)

# Hagamos una estadísticas descriptivas: 
stargazer(data.frame(data_filtrado),header=FALSE, type="text", title="Tabla 1.")

#Veamos la densidad de los datos:

# Logaritmo del salario: 
ggplot(data_filtrado, aes(x=ln_salario))+
  geom_histogram(aes(y=..density..),
                 binwidth=0.25, 
                 colour="green", fill="white")+
  geom_density(alpha=0.2, fill="orange")


# Edad: 
ggplot(data_filtrado, aes(x=age))+
  geom_histogram(aes(y=..density..),
                 bindwidth=0.25,
                 colour="green",fill="white")
#Dispersión:

ggplot(data_filtrado, aes(x=age,y=ln_salario))+
  geom_point(shape=1)+
  geom_smooth(method = lm)+
  labs(title="Dispersión entre edad y salario",
       x="Edad",
       y="Logaritmo del salario")+
  theme_bw()



