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
geih_guess <- import("stores/geih_guess.csv") #approach 1
geih_guess <- geih_guess |>
  mutate(ln_wage = log(wage),
         age_2 = age^2,
         hoursWorkUsual_2 = hoursWorkUsual^2,
         agexhours =  age*hoursWorkUsual,
         agexfimr = age*firm_time,
         iofxformal = iof1*formal,
         hoursxeducacion = hoursWorkUsual*p6210)
geih_5 <- list() ### Cambio



# Crear la receta
rec1 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                 hoursWorkUsual+ hoursWorkUsual_2 + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + agexhours + sex, data = geih_guess) |>
  step_dummy(all_nominal_predictors()) 

rec2 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                hoursWorkUsual+ hoursWorkUsual_2 + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + agexfimr + sex, data = geih_guess) #interaccion age + firm time

rec3 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                hoursWorkUsual+ hoursWorkUsual_2 + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + agexfimr + agexhours + sex, 
               data = geih_guess) # 2 interacciones age

rec4 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                hoursWorkUsual+ hoursWorkUsual_2 + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + sex, data = geih_guess)  #Sin interacciones

rec5 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                hoursWorkUsual+ hoursWorkUsual_2 + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + sex, data = geih_guess)  #Sin oficio

rec6 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                hoursWorkUsual+ hoursWorkUsual_2 + oficio + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 + sex + iofxformal, data = geih_guess)  #Interaccion iof1 + formal

rec7 <- recipe(ln_wage ~  age + age_2 + parentesco + p6210 + other_job +
                 estrato1 + cotPension + regSalud + cuentaPropia + formal +
                hoursWorkUsual+ hoursWorkUsual_2 + firm_time +
                 sizeFirm + iof1 + iof2 + iof6 +  hoursxeducacion + sex, data = geih_guess)# interaccion educ+hours

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
data_split <- initial_split(geih_guess, prop = .7)

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

#Modelo 2
fit2 <- wf2 %>%
  fit(data = train)

test_pred2 <- predict(fit2 , new_data = test) %>% 
  bind_cols(test)

test_rmse2 <- rmse(test_pred2, truth = ln_wage, estimate = .pred)
test_rmse2$.estimate

#Modelo 3

fit3 <- wf3 %>%
  fit(data = train)

test_pred3 <- predict(fit3 , new_data = test) %>% 
  bind_cols(test)

test_rmse3 <- rmse(test_pred3, truth = ln_wage, estimate = .pred)
test_rmse3$.estimate

#Modelo 4
fit4 <- wf4 %>%
  fit(data = train)

test_pred4 <- predict(fit4 , new_data = test) %>% 
  bind_cols(test)

test_rmse4 <- rmse(test_pred4, truth = ln_wage, estimate = .pred)
test_rmse4$.estimate


# Modelo 5
fit5 <- wf5 %>%
  fit(data = train)

test_pred5 <- predict(fit5 , new_data = test) %>% 
  bind_cols(test)

test_rmse5 <- rmse(test_pred5, truth = ln_wage, estimate = .pred)
test_rmse5$.estimate

#Modelo 6
fit6 <- wf6 %>%
  fit(data = train)

test_pred6 <- predict(fit6 , new_data = test) %>% 
  bind_cols(test)

test_rmse6 <- rmse(test_pred6, truth = ln_wage, estimate = .pred)
test_rmse6$.estimate

#Modelo 7
fit7 <- wf7 %>%
  fit(data = train)

test_pred7 <- predict(fit7 , new_data = test) %>% 
  bind_cols(test)

test_rmse7 <- rmse(test_pred7, truth = ln_wage, estimate = .pred)
test_rmse7$.estimate


# LOOCV Modelo 2

loocv_preds <- vector("numeric", length = nrow(geih_guess))

for (i in seq_len(nrow(geih_guess))) {
  loo_data <- geih_guess[-i, ]
  loo_fit <- wf2 %>% fit(data = loo_data)
  pred <- predict(loo_fit, new_data = slice(geih_guess, i))$.pred
  loocv_preds[i] <- pred
}

temp <-bind_cols(geih_guess$ln_wage, loocv_preds)

loocv_rmse1 <- rmse(temp, truth = ...1, estimate = ...2)

loocv_rmse1

# LOOCV Modelo 6

loocv_preds <- vector("numeric", length = nrow(geih_guess))

for (i in seq_len(nrow(geih_guess))) {
  loo_data <- geih_guess[-i, ]
  loo_fit <- wf6 %>% fit(data = loo_data)
  pred <- predict(loo_fit, new_data = slice(geih_guess, i))$.pred
  loocv_preds[i] <- pred
}

temp <-bind_cols(geih_guess$ln_wage, loocv_preds)

loocv_rmse2 <- rmse(temp, truth = ...1, estimate = ...2)

loocv_rmse2
