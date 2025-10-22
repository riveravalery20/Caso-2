library(readr)
library(dplyr)
library(tidyverse)

# Cargamos base y visualizamos

BD_EEVV_Nacimientos_2024 <- read_csv("BD-EEVV-Nacimientos-2024.csv")

# Selección y renombrado de variables

Nacimientos <- BD_EEVV_Nacimientos_2024 %>%
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
  )
# 3. Convertir variables a numéricas (importante antes de cualquier cálculo)
Nacimientos <- Nacimientos %>%
  mutate(
    Tiempo_gestación = as.numeric(Tiempo_gestación), #
    Tipo_parto = as.numeric(Tipo_parto),
    Numero_control_prenatal = as.numeric(Numero_control_prenatal), #
    Edad_madre = as.numeric(Edad_madre), #
    Numero_embarazos = as.numeric(Numero_embarazos), #
  )
#Eliminamos registros con valores 99 (sin información)

Nacimientos <- Nacimientos %>%
  filter(
    Edad_madre != 99,
    Numero_control_prenatal != 99,
    Numero_embarazos != 99,
    !Tiempo_gestación %in% c(6, 9),
    Tipo_parto != 9
  )

# Clasificar el peso

Nacimientos <- Nacimientos %>%
  mutate(
    Peso = ifelse(Peso %in% c(5, 6, 7, 8, 9), "Moderado", "Delicado")
  )

# Filtrar solo segundo semestre
Nacimientos_SegundoSemestre <- Nacimientos %>%
  mutate(Mes = as.numeric(Mes)) %>%
  filter(Mes %in% c(7, 8, 9, 10, 11, 12))

# Proporción de Bogotá en el segundo semestre
Proporcion_Bogota_SegundoSemestre <- Nacimientos_SegundoSemestre %>%
  count(Departamento) %>%
  mutate(Proporcion = n / sum(n)) %>%
  filter(Departamento == 11)

print(Proporcion_Bogota_SegundoSemestre)

# Base final de nacimientos en Bogotá 2024 segundo semestre
Nacimientos_Bogota <- Nacimientos_SegundoSemestre %>%
  filter(Departamento == 11) %>%
  mutate(Departamento = "Bogotá")
# Guardar base final de nacimientos de Bogotá (segundo semestre) en CSV
write_csv(Nacimientos_Bogota, "Nacimientos_Bogota.csv")

# Proporción interna de pesos en Bogotá
prop.table(table(Nacimientos_Bogota$Peso)) * 100

library(dplyr)

# Dividir la base según el tipo de peso
Delicados <- Nacimientos_Bogota %>% filter(Peso == "Delicado")
Moderados <- Nacimientos_Bogota %>% filter(Peso == "Moderado")

# Calcular tamaño deseado para tener 45% Delicado y 55% Moderado
total_deseado <- nrow(Delicados) / 0.45  # 45% corresponde a los delicados
tamano_moderados <- round(total_deseado * 0.55)

# Submuestrear los moderados
set.seed(123)  # Para reproducibilidad
Moderados_muestra <- Moderados %>%
  sample_n(size = tamano_moderados)

# Combinar ambas bases
Base_datos <- bind_rows(Delicados, Moderados_muestra) %>% 
  select(Tiempo_gestación,Tipo_parto,Numero_control_prenatal,Edad_madre,Numero_embarazos,
         Peso)

s# Verificar proporciones
prop.table(table(Nacimientos_Bogota_Balanceado$Peso)) * 100

# Visualizar en el visor de RStudio
View(Nacimientos_Bogota_Balanceado)

