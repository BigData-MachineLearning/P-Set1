#### 5 ###

# (a) Split the sample into two: a training (70%) and a testing (30%) sample.

#Recetas

rm(list = ls())

#Cargo los paquetes
require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, rio)


# load data
geih_clean<- import("stores/geih_clean.csv")
geih_mediana <- import("stores/geih_median.csv")
geih_media <- import("stores/geih_median.csv")
geih_guess <- import("stores/geih_guessed.csv") #approach 1

geih_5 <- list() ### Cambio

# Crear la receta
rec1 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + age*hoursWorkUsual + sex, data = geih_guess) #interaccion age + hours

rec2 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + age*firm_time + sex, data = geih_guess) #interaccion age + firm time

rec3 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + age*firm_time +  age*hoursWorkUsual + sex, 
               data = geih_guess) # 2 interacciones age

rec4 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + sex, data = geih_guess)  #Sin interacciones

rec5 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + sex, data = geih_guess)  #Sin oficio

rec6 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + sex + iof1*formal, data = geih_guess)  #Interaccion iof1 + formal

rec7 <- recipe(ln_wage ~  poly(age, 2, raw = TRUE) + parentesco + educacion + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 poly(hoursWorkUsual, 2, raw = TRUE) + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + hoursWorkUsual*educacion + sex, data = geih_guess)# interaccion educ+hours

lm_mod <- linear_reg() 

# Crear el flujo de trabajo
wf1 <- workflow() %>%
  add_recipe(rec1) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf2 <- workflow() %>%
  add_recipe(rec2) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf3 <- workflow() %>%
  add_recipe(rec3) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf4 <- workflow() %>%
  add_recipe(rec4) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf5 <- workflow() %>%
  add_recipe(rec5) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf6 <- workflow() %>%
  add_recipe(rec6) %>%
  add_model(lm_mod)

# Crear el flujo de trabajo
wf7 <- workflow() %>%
  add_recipe(rec7) %>%
  add_model(lm_mod)


set.seed(123)

# El conjunto de entrenamiento va a tener el 70% de los datos 
data_split <- initial_split(df, prop = .7)

# Creamos dos dataframes para cada uno de los conjuntos
train <- training(data_split)
test  <- testing(data_split)

# primer modelo
fit1 <- wf1 %>%
  fit(data = train)

test_pred1 <- predict(fit1 , new_data = test) %>% 
  bind_cols(test)

test_rmse1 <- rmse(test_pred1, truth = ln_wage, estimate = .pred)
test_rmse1$.estimate







