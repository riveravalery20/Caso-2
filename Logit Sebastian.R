#Librerias

library(readr)
library(dplyr)
library(tidyverse)

#Base general

BD_EEVV <- read_csv("BD-EEVV-Nacimientos-2024.csv") %>%
  select(COD_DPTO, PESO_NAC, T_GES, TIPO_PARTO, NUMCONSUL, EDAD_MADRE, N_EMB, MES) %>%
  rename(
    Departamento = COD_DPTO,
    Peso = PESO_NAC,
    Tiempo_gestación = T_GES,
    Tipo_parto = TIPO_PARTO,
    Numero_control_prenatal = NUMCONSUL,
    Edad_madre = EDAD_MADRE,
    Numero_embarazos = N_EMB,
    Mes = MES
  ) %>% 
  mutate(
    Tiempo_gestación = as.numeric(Tiempo_gestación), 
    Tipo_parto = as.numeric(Tipo_parto),
    Numero_control_prenatal = as.numeric(Numero_control_prenatal), 
    Edad_madre = as.numeric(Edad_madre), 
    Numero_embarazos = as.numeric(Numero_embarazos), 
  ) %>% 
  mutate(
    Peso = ifelse(Peso %in% c(5, 6, 7, 8, 9), "Moderado", "Delicado")
  ) %>% 
  mutate(Mes = as.numeric(Mes)) %>%
  filter(Mes %in% c(7, 8, 9, 10, 11, 12)) %>% 
  filter(Departamento == 11) %>%
  mutate(Departamento = "Bogotá")

#Balanceo

Delicados <- BD_EEVV %>% filter(Peso == "Delicado")
Moderados <- BD_EEVV %>% filter(Peso == "Moderado") %>% 
  sample_n(6000)

#Base final

Base_datos <- bind_rows(Delicados, Moderados) %>% 
  select(Tiempo_gestación,Tipo_parto,Numero_control_prenatal,Edad_madre,Numero_embarazos,
         Peso) %>% mutate(Tipo_parto=case_when(
           Tipo_parto == 1 ~ "Espontaneo",
           Tipo_parto == 2 ~ "Cesarea",
           Tipo_parto == 3 ~ "Instrumentado"
         )) %>% 
  mutate(Edad_madre=case_when(
    Edad_madre == 1 ~ "10-14 años",
    Edad_madre == 2 ~ "15-19 años",
    Edad_madre == 3 ~ "20-24 años",
    Edad_madre == 4 ~ "25-29 años",
    Edad_madre == 5 ~ "30-34 años",
    Edad_madre == 6 ~ "35-39 años",
    Edad_madre == 7 ~ "40-44 años",
    Edad_madre == 8 ~ "45-49 años",
    Edad_madre == 9 ~ "50-54 años"
  ))
