# Problem Set 1 - Big data - Machine Learning: 

# 2. Descriptive Analysis: 

# You should include a descriptive statistics table with its interpretation.
# However, I expect a deep analysis that helps the reader understand the data, its variation, 
# and the justification for your data choices. Use your professional knowledge to add value 
# to this section. Do not present it as a “dry” list of ingredients.

#Paquetes: 
require(pacman)
p_load(rvest,tidyverse, stargazer,ggplot2,dplyr)

# Importar datos: 
##geih_clean <- readRDS("C:/Users/danie/Documents/Universidad/8vo Semestre economía/Big Data y Machine Learning/Problem set 1/Datos/geih_clean.rds")

## geih_clean <- read_csv(/stores/geih_clean.csv)

view(geih_clean)

geih_clean <- geih_clean %>%
  mutate(lwage = log(wage))
##renombrar p6210 -> educacion

# Estadisticas descriptivas: 

# 1. Estadisticas laborales: 

Caract_laborales <- geih_clean[c("hoursWorkUsual","cuentaPropia","ocu","oficio","other_job",
                                          "firm_time","sizeFirm", "cotPension", "regSalud","formal")]

stargazer(as.data.frame(Caract_laborales),
          type = "text",title = "Estadisticas Descriptivas Variables Laborales",
          digits = 1, out = "table.txt",
          covariate.labels = c("Horas usual", "Cuenta propia", "Ocupado", "Oficio",
                               "Otro trabajo","Tiempo en empresa","Tamaño de firma",
                               "Pension", "Salud","formal"))
### Muy pocos cuenta propia, todos ocupados,missings Salud
# 2. Estadisticas sociodemograficas: 

caract_sociodemograficas <- geih_clean[c("sex","age", "estrato1", "parentesco", "educacion")]
  
stargazer(as.data.frame(caract_sociodemograficas),
          type = "text",title = "Estadisticas Descriptivas Variables Sociodemograficas",
          digits = 1, out = "table.txt",
          covariate.labels = c("Sexo","Edad","Estrato","Parentesto","Educacion")) 

### Edad promedio 39 años, promedio de educacion (5:secundaria incompleta). Estarto promedio de la muesta entre 2 y 3.

# 3. Estadisticas Ingresos:

Ingresos <- geih_clean[c("wage","iof1", "iof2", "iof6")]

stargazer(as.data.frame(Ingresos),
          type = "text",title = "Estadisticas Descriptivas Ingresos",
          digits = 1, out = "table.txt",
          covariate.labels = c("Salario real por hora", "Ingreso intereses - dividendos", 
                               "Ingreso pensional","Ingreso arriendos"))  

# Graficas: 

#Histogramas

# Salario - wage

ggplot(geih_clean, aes(x=wage)) + 
  geom_histogram(aes(y=..density..),binwidth=7000,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Distribucion wage",
       x = "Salario real - Hora") +
  theme_bw()
## Concentracion de los ingresos hacia la derecha. Podria ser que individuos cobran 
## salarios minimos por hora trabajada. Otro caso es que reportan salarios mas bajos
## de los que reciben ¿Incentivos? 

# Log(Salario) - lwage: 

ggplot(geih_clean, aes(x=lwage)) + 
  geom_histogram(aes(y=..density..),binwidth=.5,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Distribucion lwage",
       x = "Logaritmo salario real - Hora") +
  theme_bw() ## Distribucion mas parecidad a una normal. Se sigue evidenciando concentracion de ingresos hacia las
             ## colas de la distribucion, sobretodo hacia la derecha (salarios mas bajos).

# Horas trabajadas usualmente - hoursWorkUsual:
ggplot(geih_clean, aes(x=hoursWorkUsual)) + 
  geom_histogram(aes(y=..density..),binwidth=.5,colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Distribucion de Horas Trabajadas",
       x = "Horas trabajadas usualmente") +
  theme_bw() #Concentracion de los datos entra 40 y 48 horas. Jornada laboral
              #normal en Colombia

# Correlaciones: 

# 1. Salarios y edad (wage-age): 

ggplot(geih_clean, aes(x= age, y= wage)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Wage vs age",
       x = "Edad",
       y = "Salario") +
  theme_bw() #Correlacion Positiva. Valores atipicos. 

# 2. Log(Salarios) y edad (lwage-age): 

ggplot(geih_clean, aes(x= age, y= lwage)) +
  geom_point(shape=1) +    
  geom_smooth(method=lm) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Wage vs age",
       x = "Edad",
       y = "Ln(Salario)") +
  theme_bw() #Correlacion Positiva mas clara. #Edad maxima parece estar justo despues de los 40. 

# 3. Salario y Horas usual trabajadas (wage vs hoursWorkUsual)

ggplot(geih_clean, aes(x= hoursWorkUsual, y= wage)) +
  geom_point(shape=1) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Wage vs hoursWorkUsual",
       x = "Horas trabajadas",
       y = "Salario") +
  theme_bw() # Creciente hasta las 40-50 horas aproximadamente, despues disminuye bastante. 
            # Personas que trabajan mas de 75 horas en adelante reciben salarios muy bajos.

# 4. Log(Salario) y Horas usual trabajadas (lwage vs hoursWorkUsual)

ggplot(geih_clean, aes(x= hoursWorkUsual, y= lwage)) +
  geom_point(shape=1) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Wage vs hoursWorkUsual",
       x = "Horas trabajadas",
       y = "Ln(Salario)") +
  theme_bw() # Creciente hasta las 50 horas aproximadamente, despues disminuye. 

# 5. Salario y Tiempo en empresa (wage vs firm_time)

ggplot(geih_clean, aes(x= firm_time, y= wage)) +
  geom_point(shape=1) +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  scale_x_continuous(labels = function(x) format(x, scienticic = FALSE)) +
  labs(title = "Wage vs hoursWorkUsual",
       x = "Tiempo en la empresa",
       y = "Salario") +
  theme_bw() # Concentracion de los datos en personas que llevan poco tiempo 
            # en la empresa actual. Heterogeneidad de los datos: Puede haber
            # trabajadores que lleven poco en la empresa con salarios altos 
            # y trabajadores que llevan mucho tiempo con salarios bajos. 
            #Puede obedecer ciclos laborales
             # y contratacion estandar de contratos (1-3 años ofrecen)

# 5. Salario y Tamaño de la firma (wage vs firmsize)
box_firmsize_wage <- geih_clean |> 
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
  ## Trabajar en firmas mas grandes 

box_firmsize_wage

# 6. Salario y Educacion (wage vs educacion)
box_educ_wage <- geih_clean |> 
  mutate(p6210 = as.character(educacion)) |>
  mutate(completed = case_when(educacion == 1 ~ "Ninguno",
                               educacion == 2 ~ "Preescolar",
                               educacion == 3 ~ "Basica Primaria (1o-5o)",
                               educacion == 4 ~ "Básica secundaria (6o - 9o)",
                               educacion == 5 ~ "Media (10o - 13o) ",
                               educacion == 6 ~ "Superior o universitaria",
                               educacion == 5 ~ " 	No sabe, no informa")) |>
  ggplot(aes(x=educacion, y=lwage, fill = completed)) + 
  geom_boxplot() +
  ylab("Ln(wage)") +
  xlab("Education") +
  labs(title = "Education level vs Ln(wage)", 
       subtitle = "salario real - hourly") +
  theme_bw() +
  theme(plot.title = element_text(size = 20) , 
        plot.subtitle = element_text(size = 12, color = "#a0a0a0"),
        legend.key.size = unit(.4, 'cm'),
        legend.title = element_text(size=9))

box_educ_wage  ## Salario aumenta entre mas educacion. 

# Salarios por categoria:

# 1. Salario y pension (wage vs cotPension): 
ggplot(geih_clean, aes(x=cotPension, y=lwage)) +
  geom_bar(stat="identity") +
  labs(title = "wage vs cotPension",
       x = "Pension",
       y = "Salario")+
  theme_bw() ## 1: cotiza pension, 2: no cotiza, 3: pensionado. 
            # Individuos que cotizan pension reciben salarios mas
            # altos -> Formalidad. Pensionados tienen ingresos bajos, 
            # pueden ser diferentes a pension. 

# 2. Salario y salud (wage vs regSalud):
ggplot(geih_clean, aes(x=regSalud, y=lwage)) +
  geom_bar(stat="identity") +
  labs(title = "wage vs regSalud",
       x = "Salud",
       y = "Salario")+
  theme_bw() ## 1: contributivo, 2: especial, 3: subsidiado.
            # Individuos en regimen contributivo ganan mas salario. Pueden 
            # pagar salud y no tienen porque estar en el regimen subsidiado.
            # Regimen especial puede estar contando sobre todo profesores, sus
            # salarios no son muy altos. 

# 3. Salario y formalidad: 
ggplot(geih_clean, aes(x=formal, y=lwage)) +
  geom_bar(stat="identity") +
  labs(title = "wage vs formal",
       x = "Formal",
       y = "Salario")+
  theme_bw() +  scale_x_continuous(breaks = unique(geih_clean$formal))
#### Trabajadores formales ganan mucho mas que informales. 

# 4. Salario y sexo (wage vs sex): 

ggplot(geih_clean, aes(x=sex, y=lwage)) +
  geom_bar(stat="identity") +
  labs(title = "wage vs sex",
       x = "Sexo",
       y = "Salario")+
  theme_bw() +  scale_x_continuous(breaks = unique(geih_clean$sex))
#### 1 male, 0 female. Breacha de genero minima.




























  

