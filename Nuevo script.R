library(readr)
NacimientosV<- read_csv("BD-EEVV-Nacimientos-2024.csv")
View(NacimientosV)
NacimientosV
NacimientosV=NacimientosV %>% 
  rename(
    Peso = PESO_NAC,
    Tiempo_gestación = T_GES,
    Tipo_parto= TIPO_PARTO,
    Numero_control_prenatal= NUMCONSUL,
    Edad_madre=EDAD_MADRE,
    Numero_embarazos=N_EMB, Departamento=COD_DPTO) %>% 
  mutate(
    Peso = ifelse(
      Peso %in% c("5","6", "7", "8","9"),
      "Moderado",
      "Delicado"
    )
  )

NacimientosG <- NacimientosV %>% 
  group_by(Departamento) %>% 
  summarise(
    Total = n(),
    Moderado = sum(Peso == "Moderado"),
    Proporcion_Moderado = Moderado / Total
  )
View(NacimientosG)
