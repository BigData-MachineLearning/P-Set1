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

# concentracion esperada entre las 40 y 48 horas que son usuales en los contratos, 
# con un numero no negligible cerca de las 60

# Wages vs hours worked

scatt_wage_hours <- ggplot(geih2018, aes(x = hoursWorkUsual, y = lwage)) +
  geom_point(shape=1, color = "#FF6666", alpha = 0.5) +   
  geom_smooth(method=lm,  
              se=FALSE, colour="black") + 
  labs(title = "Hours worked vs Ln(wages)", 
       subtitle = "Usual weekly and salary - real hourly") +
  ylab("Ln(wages)") +
  xlab("Hours worked") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

scatt_wage_hours

ggsave("views/scatt_wage_hours.png")

# Una relacion algo similar a la de edada, al principio es ligeramente creciente pero
# Parece que después de las 50 horas no hay mucho benefioco en temas salariares
# Posible paradigma cuadratico

# distribution size firms

bar_firmsize <- geih2018 |> 
  mutate(firm_size = case_when(sizeFirm == 1 ~ "Self - employed",
                               sizeFirm == 2 ~ "2-5 workers",
                               sizeFirm == 3 ~ "6-10 workers",
                               sizeFirm == 4 ~ "11-50",
                               sizeFirm == 5 ~ ">50")) |> 
  ggplot(aes(x = sizeFirm, fill = firm_size)) + 
  geom_histogram(binwidth = 1, color = "black") +
  xlab("Firm Size") +
  ylab("Number of firms") +
  labs(title = "Firm Size distribution",
       subtitle = "based on number of employees") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

bar_firmsize

ggsave("views/bar_firmsize.png")

# al ser datos de Bogota, habra sobre todo enoresas gransesm micro empresas y cuentapropias, 
# En el resto del pais serían mas micro y cuenta propia que grandes
 
#size firm vs salario

box_firmsize_wage <- geih2018 |> 
  mutate(sizeFirm = as.character(sizeFirm)) |>
  mutate(firm_size = case_when(sizeFirm == 1 ~ "Self - employed",
                               sizeFirm == 2 ~ "2-5 workers",
                               sizeFirm == 3 ~ "6-10 workers",
                               sizeFirm == 4 ~ "11-50",
                               sizeFirm == 5 ~ ">50")) |> 
  ggplot( aes(x=sizeFirm, y=lwage, fill = firm_size)) + 
  geom_boxplot() +
  theme_bw()+
  labs(title = "Firm Size vs Ln(wage)", 
       subtitle = "based on number of employees and salary - real hourly") +
  xlab("Firm Size") +
  ylab("Ln(wages)") +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_firmsize_wage

ggsave("views/box_firmsize_wage.png")
  

#Personas que trabajan en empresa mas grande ganan mas en promedio

# formal vs wage

box_formal_wage <- geih2018 |> 
  mutate(formal = as.character(formal)) |>
  ggplot( aes(x=formal, y=lwage, fill = formal)) + 
  geom_boxplot() +
  labs(title = "Formality vs Ln(wage)", 
       subtitle = "(0 informal and 1 formal) and salary - real hourly") +
  xlab("Formality") +
  ylab("Ln(wages)") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_formal_wage

ggsave("views/box_formal_wage.png")

#Los formales ganan mas logicamente


# sex vs wage
box_sex_wage <- 

box_sex_wage <- geih2018 |>
  mutate(sex = as.character(sex)) |>
  ggplot( aes(x=sex, y=lwage, fill = sex)) + 
  geom_boxplot() +
  labs(title = "Sex vs Ln(wage)", 
       subtitle = "(1 male - 0 female) and salary - real hourly") +
  xlab("Sex") +
  ylab("Ln(wages)") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

box_sex_wage
ggsave("views/box_sex_wage.png")

#### 1 male, 0 female. Breacha de genero minima.



# Firm time distribution

hist_firmtime <- geih2018 |> mutate(firm_time = firm_time/12) |># pasar a años
  ggplot(aes(x=firm_time)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=1,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")  + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
  xlab("Time in firm") +
  labs(title = "Time in firm - distribution",
       subtitle = "in years") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

hist_firmtime

ggsave("views/hist_firmtime.png")

#Concentrado en tiempos cortos y picos en cada 10 años- Puede obedecer ciclos laborales
# y contratacion estandar de contratos (1-3 años ofrecen)

#Firm time vs wage
scatt_wage_firmtime <- geih2018 |> 
  mutate(firmtime = firm_time/12) |>
  ggplot(aes(x = firmtime, y = lwage)) +
  geom_point(shape=1, alpha=.5, color="#FF6666") +    # Use hollow circles
  geom_smooth(method=lm,   # Add linear regression line
              se=FALSE, color = "black")  + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scientific = FALSE))+
  xlab("Time in firm") +
  ylab("Ln(wage)") +
  labs(title = "Time in firm vs Ln(wage)",
       subtitle = "in years and salary - real hourly") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0")) 

scatt_wage_firmtime

ggsave("views/scatt_wage_firmtime.png")

#Parece ser que sí se relacionan positivamente


#Educ Level and salary

box_educ_wage <- geih2018 |> 
  mutate(p6210 = as.character(p6210)) |>
  mutate(completed = case_when(p6210 == 1 ~ "Ninguno",
                               p6210 == 2 ~ "Preescolar",
                               p6210 == 3 ~ "Basica Primaria (1o-5o)",
                               p6210 == 4 ~ "Básica secundaria (6o - 9o)",
                               p6210 == 5 ~ "Media (10o - 13o) ",
                               p6210 == 6 ~ "Superior o universitaria",
                               p6210 == 5 ~ " 	No sabe, no informa")) |>
  ggplot(aes(x=p6210, y=lwage, fill = completed)) + 
  geom_boxplot() +
  ylab("Ln(wage)") +
  xlab("Education") +
  labs(title = "Education level vs Ln(wage)", 
       subtitle = "salary - real hourly") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0"),
        legend.key.size = unit(.4, 'cm'),
        legend.title = element_text(size=9))

box_educ_wage

ggsave("views/box_educ_wage.png")

# Sube con la educacion, cambio mas notable superior,
#pero mayor Rango intercuartil en educacion superior

