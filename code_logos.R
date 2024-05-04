# Script para analisis de base de datos logos SOCIMEP
# Hecho por: Frank Zela-Coila
# Fecha: 08-04-2024



# Paquetes
library(tidyverse)
library(openxlsx)
library(compareGroups)
library(Hmisc)

# Carga de la base de datos----
df <- read.xlsx("data/BD_LOGOS.xlsx")
str(df)
# Variables
# Categorizacion
str(df)
names(df)
table(df$correcto_uso)
#df$correcto_uso <- factor(df$correcto_uso,
 #                        levels = c(0,1), #0 es correcto y 1 incorrecto
  #                       labels = c(1,2)) # ahora 1 es correcto y 0 incorrecto


df <- df %>% 
  mutate(anti10 = case_when(
    antigüedad < 10 ~ "Menores de 10 a",
    antigüedad >= 10 & antigüedad < 20 ~ "10 a 20 a",
    antigüedad >= 20 ~ "Más de 20 a",
    TRUE ~ "no answer" ) )   

df <- df %>% 
  mutate(anti20 = case_when(
    antigüedad >= 20 ~ "Más de 20 a",
    antigüedad < 20 ~ "Menores de 20 a",
    TRUE ~ "no answer" ) ) 
names(df)
df$anti20 <- factor(df$anti20,
                        levels = c("Menores de 20 a","Más de 20 a"))
table(df$anti20)

# Tablas

df_tabla1 <- df %>% 
  select(code, consejeria, tipo_u, 
         tipo_socem, antigüedad, anti10,anti20, tipo_log,
         tipo_uso, correcto_uso, n_serpi,
         manu_ide)

df_tabla1

tabla1 <- createTable(compareGroups(data=df_tabla1,
                                    method = c(3, 3, 3, 
                                               3, 2, 3, 3,3, 
                                               3, 3, 3, 
                                               3)))

tabla1

export2xls(tabla1, "table/tabla1.xlsx")

tabla2 <- createTable(compareGroups(correcto_uso ~ consejeria + tipo_u + 
                                    tipo_socem + antigüedad + anti20 + tipo_log +
                                    tipo_uso + n_serpi +
                                    manu_ide,
                                    data=df_tabla1,
                                    byrow = T,
                                    method = c(3, 3, 3, 
                                               2, 3, 3, 
                                               3, 3, 3, 
                                               3)))

tabla2
export2xls(tabla2, "table/tabla2.xlsx")



library(sjPlot)



df_tabla1$n_serpi <- as.factor(df_tabla1$n_serpi)
table(df_tabla1$correcto_uso)



model1 <- glm(data=df_tabla1, 
              formula = correcto_uso ~ consejeria + tipo_u + 
                tipo_socem + anti20 +
                manu_ide,
              family = poisson(link = "log"))

model2 <- glm(data=df_tabla1, 
              formula = correcto_uso ~ consejeria + tipo_u + 
                tipo_socem + antigüedad + n_serpi +
                manu_ide,
              family = "binomial")



sjPlot::tab_model(model1)
sjPlot::tab_model(model2)
summary(model1)
library(sjPlot)
tab_model(model1)

RP.Poisson <- function(modelo) 
{
  library(sandwich)
  m1 <- modelo
  cov.m1 <- vcovHC(m1, type="HC0")
  std.err <- sqrt(diag(cov.m1))
  r.est <- cbind(Estimate= coef(m1), "Robust SE" = std.err,
                 "Pr(>|z|)" = 2 * pnorm(abs(coef(m1)/std.err), lower.tail=FALSE),
                 LL = coef(m1) - 1.96 * std.err,
                 UL = coef(m1) + 1.96 * std.err,
                 "RP" = exp(coef(m1)),
                 "LCL" = exp(coef(m1) - 1.96 * std.err),
                 "UCL" = exp(coef(m1) + 1.96 * std.err))
  return(r.est)
  rm(m1,cov.m1,std.err,r.est)
}


RP.Poisson(model1)

model1 <- as.data.frame(RP.Poisson(model1))

model1$variable <- rownames(model1)

rownames(model1) <- NULL
model1

write.xlsx(model1, "table/tabla3.xlsx")


# Comites
tabla_comite <- df %>% 
  select(code, correcto_uso, sim_CPA, sim_CPC, sim_CPPC, 
         sim_CPDII, sim_CPRII, sim_CPAIS)

dim(tabla_comite)
names(tabla_comite)
tabla_comite
tabla_comite <- gather(tabla_comite, "comite", "simbolo", 3:8)
names(tabla_comite)

tabla_comite <- tabla_comite %>% 
  mutate(cor_uso = case_when(
    simbolo %in% c(0, 2, 3) ~ "Si",
    simbolo %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  

names(tabla_comite)

tabla1_comite <- createTable(compareGroups(data=tabla_comite,
                                           method = c(3, 3, 3, 
                                                      3, 3, 3, 3)))

tabla1_comite
names(tabla_comite)
tabla2_comite <- createTable(compareGroups(comite ~ cor_uso + simbolo,
                                    data=tabla_comite,
                                    byrow = F,
                                    method = c(3, 3), max.ylev=6),
                             )
tabla2_comite
export2xls(tabla2_comite, "table/tabla1_comite.xlsx")

##### mutate - no usar----
tabla_comite <- tabla_comite %>% 
  mutate(cor_CPA = case_when(
    sim_CPA %in% c(0, 2, 3) ~ "Si",
    sim_CPA %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  

tabla_comite <- tabla_comite %>% 
  mutate(cor_CPC = case_when(
    sim_CPC %in% c(0, 2, 3) ~ "Si",
    sim_CPC %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  

tabla_comite <- tabla_comite %>% 
  mutate(cor_CPPC = case_when(
    sim_CPPC %in% c(0, 2, 3) ~ "Si",
    sim_CPPC %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  

tabla_comite <- tabla_comite %>% 
  mutate(cor_CPDII = case_when(
    sim_CPDII %in% c(0, 2, 3) ~ "Si",
    sim_CPDII %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  

tabla_comite <- tabla_comite %>% 
  mutate(cor_CPRII = case_when(
    sim_CPRII %in% c(0, 2, 3) ~ "Si",
    sim_CPRII %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  

tabla_comite <- tabla_comite %>% 
  mutate(cor_CPAIS = case_when(
    sim_CPAIS %in% c(0, 2, 3) ~ "Si",
    sim_CPAIS %in% c(1, 4, 5) ~ "No",
    TRUE ~ NA ) )  
#Otros ----

# secuencias aleatorias provicionales
# Tipo de universidad
tip_u <- c("Privada", "Nacional")
tipo_uso <- c("ET", "EV","HT", "HV","CE", "Otro")
n_serpi <- c(0,1,2)
manu_ide <- c("No", "Si")

# secuencia aleatoria
set.seed(123)  # Esto asegura reproducibilidad
df$tipo_u <- sample(tip_u, 41, replace = TRUE)
df$tipo_uso <- sample(tipo_uso, 41, replace = TRUE)
df$n_serpi <- sample(n_serpi, 41, replace = TRUE)
df$manu_ide <- sample(manu_ide, 41, replace = TRUE)
str(df)
