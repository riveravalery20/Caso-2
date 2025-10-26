library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(ISLR)

# Base general
BD_EEVV <- read_csv("BD-EEVV-Nacimientos-2024.csv") %>%
  select(COD_DPTO, PESO_NAC, T_GES, TIPO_PARTO, NUMCONSUL, EDAD_MADRE, N_EMB, MES) %>%
  rename(
    Departamento = COD_DPTO,
    Peso_delicado = PESO_NAC,
    Tiempo_gestación = T_GES,
    Tipo_parto = TIPO_PARTO,
    Numero_control_prenatal = NUMCONSUL,
    Edad_madre = EDAD_MADRE,
    Numero_embarazos = N_EMB,
    Mes = MES
  ) %>%
  mutate(
    Numero_control_prenatal = as.numeric(Numero_control_prenatal),
    Numero_embarazos = as.numeric(Numero_embarazos),
    # Convertimos Tiempo_gestación a factor con etiquetas claras
    Tiempo_gestación = factor(Tiempo_gestación,
                              levels = c(1, 2, 3, 4, 5),
                              labels = c("22 o menos semanas", "22-27 semanas", "28-37 semanas",
                                         "De 38-41 semanas", "42 o más semanas"))
  ) %>%
  mutate(
    Peso_delicado = ifelse(Peso_delicado %in% c(5, 6, 7, 8, 9), "No", "Si"),
    Peso_delicado = factor(Peso_delicado, levels = c("Si", "No"))
  ) %>%
  mutate(Mes = as.numeric(Mes)) %>%
  filter(Mes %in% c(7, 8, 9, 10, 11, 12)) %>%
  filter(Departamento == 11) %>%
  mutate(Departamento = "Bogotá") %>%
  na.omit()

# Balanceo
set.seed(5)

Si <- BD_EEVV %>% filter(Peso_delicado == "Si")
No <- BD_EEVV %>% filter(Peso_delicado == "No") %>%
  sample_n(6000)

# Base final
Base_datos <- bind_rows(Si, No) %>%
  select(Tiempo_gestación, Tipo_parto, Numero_control_prenatal,
         Edad_madre, Numero_embarazos, Peso_delicado) %>%
  mutate(
    Tipo_parto = case_when(
      Tipo_parto == 1 ~ "Espontaneo",
      Tipo_parto == 2 ~ "Cesarea",
      Tipo_parto == 3 ~ "Instrumentado"
    ),
    Edad_madre = case_when(
      Edad_madre == 1 ~ "10-14 años",
      Edad_madre == 2 ~ "15-19 años",
      Edad_madre == 3 ~ "20-24 años",
      Edad_madre == 4 ~ "25-29 años",
      Edad_madre == 5 ~ "30-34 años",
      Edad_madre == 6 ~ "35-39 años",
      Edad_madre == 7 ~ "40-44 años",
      Edad_madre == 8 ~ "45-49 años",
      Edad_madre == 9 ~ "50-54 años"
    ),
    Tipo_parto = as.factor(Tipo_parto),
    Edad_madre = as.factor(Edad_madre)
  ) %>%
  filter(complete.cases(.))

# Verificar proporciones
prop.table(table(Base_datos$Peso_delicado)) * 100

# Visualizar en el visor de RStudio
View(Base_datos)

