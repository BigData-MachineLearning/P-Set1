### Punto 3 ### primera reg

# Age-wage profile. A great deal of evidence in Labor economics suggests that the
# typical worker’s age-wage profile has a predictable path: “Wages tend to be low when
# the worker is young; they rise as the worker ages, peaking at about age 50; and the
# wage rate tends to remain stable or decline slightly after age 50”.
# In this subsection we are going to estimate the Age-wage profile profile for the indi-
#   viduals in this sample:



# llamar paquetes

require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, boot)


# cargo csv
geih2018 <- read_csv("C:/Users/jorge_j24fcle/OneDrive/Documentos/universidad/NOVENO SEMESTRE/BIG DATA/geih_clean.csv")

geih2018 <- geih2018 |>
  mutate(lwage = log(wage))


#Corro el  modelo
mod1 <- lm(lwage ~poly(age, 2, raw = TRUE), data = geih2018)

#Tabla
stargazer(mod1, type = 'text')

#boostraap CI para max age

set.seed(123)
confi_age <- function(data, index){
  f <- lm(lwage ~ poly(age, 2, raw = TRUE), data = data, subset = index)
  coefs <- f$coefficients
  b1<-coefs[2]
  b2<-coefs[3] 
  
  max_age <- -(b1/(2*b2))
  return(max_age)
 
}
max_age <-boot(geih2018, confi_age, R = 3000)

agelims = geih2018 %>%
  select(age) %>%
  range

# genero edades para graficar el modleo
age_grid <- seq(from = min(agelims), to = max(agelims))


# Genero predicciones para edades generadas para graficar mi modelo

preds <- predict(mod1, newdata = list(age = age_grid), se = TRUE)

# CI modelo predicho
se_bands <- cbind("upper" = preds$fit+1.96*summary(mod1)$sigma, 
                 "lower" = preds$fit-1.96*summary(mod1)$sigma)
se_bands
#Grafica
ggplot() +
  geom_line(aes(x = age_grid, y = preds$fit), color = "black") +
  geom_ribbon(aes(x = age_grid, 
                  ymin = se_bands[,"lower"], 
                  ymax = se_bands[,"upper"]), 
              alpha = 0.3) +
  xlim(agelims) +
  labs(title = "Degree-2 Polynomial") +
  geom_vline(xintercept=max_age$t0[[1]],linetype=2) +
  theme_bw() +
  geom_line(aes(x = age_grid, y = se_bands[,"lower"]), col = "coral2", linetype = "dashed") + #lwr pred interval
  geom_line(aes(x = age_grid,y = se_bands[,"upper"]), col = "coral2", linetype = "dashed") #upr pred interval

summary(mod1)