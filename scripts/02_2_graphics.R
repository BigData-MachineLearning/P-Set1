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

# distribution of wage

#histograma de wage


hist_wage <- ggplot(geih2018, aes(x=wage)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=7000,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Distribution - Wages", subtitle = "salary - real hourly") +
  xlab("wages") +
  theme_bw()+
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0"))

hist_wage

ggsave("views/hist_wage.png")


# Es un grafico muy concentrado en ingresos relativamente bajos, parece ser que
# se acerca bastante a lo que por hora cobraría un mínimo mentras existen otros muy lejanos
# No se aprecia muy bien

#histograma log wage

hist_logwage <- ggplot(geih2018, aes(x=lwage)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title = "Distribution - Ln(wages)", subtitle = "salary - real hourly") +
  xlab("Ln(wages)") +
  theme_bw()+
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0"))

hist_logwage

ggsave("views/hist_logwage.png")

# El logaritmo "normaliza" la distribucion pero seguimos viendo que hay una gran concentracion en
# unos valores con colas bajas y lejanas a lo que es la media.

# wage vs age

scatt_wage_age <- ggplot(geih2018, aes(x=age, y=lwage)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, color = "black")   + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  labs(title = "Age vs Ln(wages)", subtitle = "salary - real hourly") +
  ylab("Ln(wages)") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0"))

scatt_wage_age

ggsave("views/scatt_wage_age.png")

# Se observa una relacion un poco creciente en lso primeros años que se aplana despues de los 50,
# mas adelante se detallará


#Hours worked distribution
#histogram hours worked

hist_hours <- ggplot(geih2018, aes(x=hoursWorkUsual)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=2.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) + 
  labs(title = "Distribution - Hours worked", subtitle = "Usual weekly") +
  xlab("Hours worked") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0"))

hist_hours

ggsave("views/hist_hours.png")