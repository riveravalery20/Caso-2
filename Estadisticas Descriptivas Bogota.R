# 1. Cargar librerías
library(readr)
library(dplyr)
library(class)

# 2. Cargar la base de datos
BD_EEVV_Nacimientos_2024 <- read_csv("BD-EEVV-Nacimientos-2024.csv")

# Selección y renombrado de variables relevantes
Nacimientos <- BD_EEVV_Nacimientos_2024 %>%
  select(COD_DPTO, PESO_NAC, T_GES, TIPO_PARTO, NUMCONSUL, EDAD_MADRE, N_EMB, MES) %>%
  rename(
    Departamento = COD_DPTO,
    Peso = PESO_NAC,
    Tiempo_gestacion = T_GES,
    Tipo_parto = TIPO_PARTO,
    Numero_control_prenatal = NUMCONSUL,
    Edad_madre = EDAD_MADRE,
    Numero_embarazos = N_EMB,
    Mes = MES
  ) %>%
  mutate(
    Tiempo_gestacion = as.numeric(Tiempo_gestacion),
    Tipo_parto = as.numeric(Tipo_parto),
    Numero_control_prenatal = as.numeric(Numero_control_prenatal),
    Edad_madre = as.numeric(Edad_madre),
    Numero_embarazos = as.numeric(Numero_embarazos)
  )

# Clasificar el peso del recién nacido
Nacimientos <- Nacimientos %>%
  mutate(Peso = ifelse(Peso %in% c(5, 6, 7, 8, 9), "Moderado", "Delicado"))

# Filtrar solo segundo semestre y Bogotá
Nacimientos_SegundoSemestre <- Nacimientos %>%
  filter(as.numeric(Mes) %in% 7:12)

Nacimientos_Bogota <- Nacimientos_SegundoSemestre %>%
  filter(Departamento == 11) %>%
  mutate(Departamento = "Bogotá")