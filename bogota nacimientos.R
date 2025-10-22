library(readr)
library(dplyr)

# Cargamos base y visualizamos

BD_EEVV_Nacimientos_2024 <- read_csv("BD-EEVV-Nacimientos-2024.csv")
View(BD_EEVV_Nacimientos_2024)

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
View(Nacimientos_SegundoSemestre)

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
View(Nacimientos_Bogota)

# Proporción interna de pesos en Bogotá
prop.table(table(Nacimientos_Bogota$Peso)) * 100


