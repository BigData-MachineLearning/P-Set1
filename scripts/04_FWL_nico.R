# We are going to develope the FWL method with and without bootstrap in this R script:
rm(list = ls())
require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, rio)

# First, we need our data:

geih_2018 <- read_csv("stores/geih_clean.csv")
geih_clean <- read_csv("stores/geih_clean.csv")

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
reg_1 <-  lm(wage ~ ., data = geih_clean )


# Step 3: Usage of the predict command to predict the missing values.
predicted_values <- predict(reg_1, newdata = data_with_missing)

# Step 4: Add the predictions.
geih1[is.na(geih1$wage), "wage"] <- predicted_values # by now, we have reduced the missing values up
# to 837.
geih1 <- geih1 %>% filter(!is.na(wage)& wage>0)

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

#We now run the model as proposed:

model <- lm(ln_wage~female, data=geih_final) #We run the model using only wage and female. Female is a 
# dummy variable which is 1 if female, 0 if male. 

stargazer(model, type = 'text')


# Now, let's apply the FWL theorem.

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







