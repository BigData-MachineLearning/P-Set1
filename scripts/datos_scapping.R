# En este script vamos a importar la base de datos del Problem Set 1 de Big Data.

# Limpio mi espacio de trabajo
rm(list = ls())

#Cargo los paquetes
require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, rio)

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

#Revisar obs unicas
nrow(unique(geih[c("directorio", "secuencia_p", "orden")]))

# Filtremos el DataFrame para seleccionar únicamenta aquellas personas ocupadas y cuya edad sea mayor o igual a 18. 
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
         firm_time = p6426) |>
  mutate(age2=age*age, ln_wage=log(wage))

  #Meto en data frame
  
  geih_clean <- as.data.frame(geih_clean)
  
  #Manejo de missings
  
  # Tenemos 6650 missing en mi variable de interes
  
  geih1 <- geih_clean
  geih2 <- geih_clean
  geih3 <- geih_clean
  # Approach 1: imputar con predict
  
  # Step 1: Dividir en datos con missing y sin missing
  data_with_missing <- geih1[is.na(geih1$wage), ]  # Rows with missing values
  data_without_missing <- geih1[!is.na(geih1$wage), ]  # Rows without missing values
  
  # Step 2: regression en sector sin missings
  reg_1 <-  lm(wage ~ ., data = geih1 )
  
  
  # Step 3: Uso el predict para predecir faltantes
  predicted_values <- predict(reg_1, newdata = data_with_missing)
  
  # Step 4: imputo los predichos
  geih1[is.na(geih1$wage), "wage"] <- predicted_values
  
  # Approach 2: imputar promedio
  
  geih2$wage[is.na(geih2$wage)] <- mean(geih2$wage, na.rm = TRUE)
  
  # Approach 3: imputar mediana por cola a la derecha
  
  geih3$wage[is.na(geih3$wage)] <- median(geih3$wage, na.rm = TRUE)
  
  # Revisamos el proceso 
  colSums(is.na(geih1)) # de 6650 a 837 quito los que son missings
  
  colSums(is.na(geih2)) # de 6650 a 0
  
  colSums(is.na(geih3)) # de 6650 a 0
  
  
  #Guardare los datos Paro no tener que correr el scrapping otra vez.
  
  export(geih, "stores/geih_complete.csv") #complete scrapped element
  export(geih_clean, "stores/geih_clean.csv") #selected variables and lnwage

  export(geih1, "stores/geih_guess.csv") # geih with predicted NA
  export(geih2, "stores/geih_mean.csv") # mean in NA
  export(geih3, "stores/geih_median.csv") #median in NA
    
  
  

  