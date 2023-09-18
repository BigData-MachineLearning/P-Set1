# We are going to develope the FWL method with and without bootstrap in this R script:
rm(list = ls())
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer, boot, car, flextable)

# First, we need our data:

geih_2018 <- read_csv("stores/geih_clean.csv")
geih_clean <- read_csv("stores/geih_clean.csv")
geih_clean <- geih_clean %>% 
  select(!ln_wage)

# At this point, we need to decide which method we're going to use to deal with the missing values.

colSums(is.na(geih_clean))

# There's a total of 6050 missing values in our variable of interest (wage), and so, we discuss 
# the following approaches: 

geih1 <- geih_clean #this data will be used to develope the first approach.
geih2 <- geih_clean #this data will be used to develope the second approach.
geih3 <- geih_clean #this data will be used to develope the third approach.


############### Approach 1: use predict to fill missing values: #################

# Step 1: divide data with missing and without missing values.

data_with_missing <- geih_clean[is.na(geih_clean$wage), ]  # Rows with missing values
data_without_missing <- geih_clean[!is.na(geih_clean$wage), ]  # Rows without missing values

# Step 2: regression in without missing values. 
reg_1 <- lm(wage ~ ., data = geih_clean )


# Step 3: Usage of the predict command to predict the missing values.
predicted_values <- predict(reg_1, newdata = data_with_missing)

# Step 4: Add the predictions.
geih1[is.na(geih1$wage), "wage"] <- predicted_values # by now, we have reduced the missing values up
# to 837.
geih1 <- geih1 %>% filter(!is.na(wage)& wage>0)
export(geih1, "stores/geih_guess.csv")

############## Approach 2: Using mean:###############

geih2$wage[is.na(geih2$wage)] <- mean(geih2$wage, na.rm = TRUE) # we set all missing values to the
# wage mean. 

# ############Approach 3: Using median: #############

geih3$wage[is.na(geih3$wage)] <- median(geih3$wage, na.rm = TRUE)

#After some discussion, we decided to use the first approach for this regard.

geih_final <- geih1 #### Here we select the approach we would like to use.

geih_final <- geih_final%>% mutate(ln_wage=log(wage),
                                 female=case_when(sex==1~0,
                                                  sex==0~1))

#A. We now run the model as proposed:

model <- lm(ln_wage~female, data=geih_final) #We run the model using only wage and female. Female is a 
# dummy variable which is 1 if female, 0 if male. 

stargazer(model, type = 'text')



#B. Now, let's apply the FWL theorem.

#1. Using our variable of interest and some controls.

reg1 <- lm(female ~ poly(age,2,raw=TRUE)+ parentesco + p6210 + other_job + estrato1 +
             cotPension + regSalud + cuentaPropia + formal + poly(hoursWorkUsual,2,raw=TRUE) + 
             oficio + firm_time + sizeFirm + iof1 + iof2 + iof6, data=geih_final)

stargazer(reg1,type="text")

#2. We save the residuals of the model above.
residualsreg1 <- resid(reg1)

#3. Now we regress our model with our variable of interest:

reg2 <- lm(ln_wage ~ poly(age,2,raw=TRUE)+ parentesco + p6210 + other_job + estrato1 +
             cotPension + regSalud + cuentaPropia + formal + poly(hoursWorkUsual,2,raw=TRUE) + 
             oficio + firm_time + sizeFirm + iof1 + iof2 + iof6, data=geih_final)

stargazer(reg2,type="text")

#4. We save the residuals aswell.

residualsreg2 <- resid(reg2)

#5. We regress both residuals:

reg3 <- lm(residualsreg2 ~ residualsreg1, data=geih_final)

stargazer(model,reg3,type="text")

#Let's to the FWL again, but using bootstrap.
set.seed(123)  
modelo_boot <- function(data, index) {
  f <- lm(female ~ poly(age, 2, raw = TRUE) + parentesco + p6210 + other_job +
            estrato1 + cotPension + regSalud + cuentaPropia + formal +
            poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
            sizeFirm + iof1 + iof2 + iof6, data = data, subset = index)
  coefs <- f$coefficients
  
  residuales <- resid(f)
  
  f2 <- lm(ln_wage ~ poly(age, 2, raw = TRUE) + parentesco + p6210 + other_job +
             estrato1 + cotPension + regSalud + cuentaPropia + formal +
             poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
             sizeFirm + iof1 + iof2 + iof6, data = data, subset = index)
  coefs2 <- f2$coefficients
  
  residuales2 <- resid(f2)
  
  f3 <- lm(residuales2 ~ residuales, data = data, subset = index)
  
  return(coef(f3))
  #residuales_ponderados <- residuales / sqrt(weights)
  
  #return(list(coefs = coefs, residuales_ponderados = residuales_ponderados))
}

bootstrap_results <- boot(geih_final,modelo_boot, R=1000)
bootstrap_results

##Graph
#boostraap CI para max age
set.seed(123)
confi_age <- function(data, index){
  f <- lm(ln_wage ~ poly(age, 2, raw = TRUE)+ female + age*female, data = data, subset = index)
  coefs <- f$coefficients
  b1<-coefs[2]
  b2<-coefs[3] 
  b3<-coefs[4]
  b4<-coefs[6]
  
  max_agef <- -((b1+b4)/(2*b2))
  
  max_agem <- -(b1/(2*b2))
  
  max_ages <- c(max_agef,max_agem)
  return(max_ages)
}


max_age <-boot(geih_final, confi_age, R = 1000)
max_age

mod1<-lm(ln_wage ~ poly(age, 2, raw = TRUE)+ female + age*female, data = geih_final)

agelims = geih_final %>%
  select(age) %>%
  range

# genero edades para graficar el modleo
age_grid <- seq(from = min(agelims), to = max(agelims))
mujer <- rep(1,length(age_grid))
hombre <- rep(0,length(age_grid))
# Genero predicciones para edades generadas para graficar mi modelo

predsf <- predict(mod1, newdata = list(age = age_grid,female=mujer), se = TRUE)

predsm <- predict (mod1, newdata = list(age = age_grid,female=hombre), se = TRUE)

# CI modelo predicho
se_bands_f <- cbind("upper" = predsf$fit+1.96*summary(mod1)$sigma, 
                  "lower" = predsf$fit-1.96*summary(mod1)$sigma)
se_bands_m <- cbind("upper" = predsm$fit+1.96*summary(mod1)$sigma, 
                    "lower" = predsm$fit-1.96*summary(mod1)$sigma)

#Grafica
ggplot() +
  geom_line(aes(x = age_grid, y = predsf$fit), color = "#fe6a6a") +
  geom_line(aes(x = age_grid, y = predsm$fit), color = "#6a9dfe") +
  xlim(agelims) +
  labs(title = "Degree-2 Polynomial") +
  geom_vline(xintercept=max_age$t0[[1]], linetype=2) +
  theme_bw() +
  geom_line(aes(x = age_grid, y = se_bands_f[,"lower"]), col = "#fe6a6a", linetype = "dashed") + #lwr pred interval
  geom_line(aes(x = age_grid,y = se_bands_f[,"upper"]), col = "#fe6a6a", linetype = "dashed") + #upr pred interval 
  geom_line(aes(x = age_grid, y = se_bands_m[,"lower"]), col = "#6a9dfe", linetype = "dashed") + #lwr pred interval
  geom_line(aes(x = age_grid,y = se_bands_m[,"upper"]), col = "#6a9dfe", linetype = "dashed") #upr pred interval

summary(mod1)