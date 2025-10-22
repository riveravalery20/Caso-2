library(readr)
library(dplyr)

BD_EEVV_Nacimientos_2024 <- read_csv("BD-EEVV-Nacimientos-2024.csv")
#seleccion de variables de 

Nacimientos <- BD_EEVV_Nacimientos_2024 %>%
  select(COD_DPTO, PESO_NAC, T_GES, TIPO_PARTO, NUMCONSUL, EDAD_MADRE, N_EMB) %>%
  rename(
    Departamento = COD_DPTO,
    Peso = PESO_NAC,
    Tiempo_gestación = T_GES,
    Tipo_parto = TIPO_PARTO,
    Numero_control_prenatal = NUMCONSUL,
    Edad_madre = EDAD_MADRE,
    Numero_embarazos = N_EMB
  )

# 4. Clasificar el peso como Moderado o Delicado

Nacimientos <- Nacimientos %>%
  mutate(
    Peso = ifelse(
      Peso %in% c(5, 6, 7, 8, 9),
      "Moderado",
      "Delicado"
    )
  )


#proporción de Bogotá en los datos totales

Proporcion_Bogota <- Nacimientos %>%
  count(Departamento) %>%
  mutate(Proporcion = n / sum(n)) %>%
  filter(Departamento == 11)

print(Proporcion_Bogota)

# Base final de nacimientos en bogota 

Nacimientos_Bogota <- Nacimientos %>%
  filter(Departamento == 11) %>%   # 11 = Bogotá
  mutate(Departamento = "Bogotá")

View(Nacimientos_Bogota)
prop.table(table(Nacimientos_Bogota$Peso)) * 100
