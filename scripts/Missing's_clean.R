##Limpieza de Missing values

rm(list = ls())

require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer, boot, car, flextable)

geih_clean <- read_excel("C:/Users/Usuario/Documents/GitHub/Machine-Learning/geih_clean.xlsx")

geih_clean <- geih_clean %>%
  mutate(female= case_when(sex ==1 ~0, sex==0~1))


##Camos a imputar mediana por cola a la derecha
geih_clean$wage[is.na(geih_clean$wage)] <- median(geih_clean$wage, na.rm = TRUE)

colSums(is.na(geih_clean))

write.xlsx(geih_clean, "geih_mediana")

