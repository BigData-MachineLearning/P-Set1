### Punto 2.2 ### 

# A descriptive analysis of the data. At a minimum, you should include a descrip-
# tive statistics table with its interpretation. However, I expect a deep analysis
# that helps the reader understand the data, its variation, and the justification for
# your data choices. 

# Use your professional knowledge to add value to this section.
# Do not present it as a “dry” list of ingredients


geih2018 <- read_csv("stores/geih_clean.csv")

colnames(geih2018)

geih2018 <- as.data.frame(geih2018)
geih2018 <- geih2018 |>
  mutate(lwage = log(wage))

require(pacman)
p_load(rvest, tidyverse, knitr, kableExtra, readxl,
       skimr, tidymodels, stargazer, broom, boot)

# Now we take a closer look