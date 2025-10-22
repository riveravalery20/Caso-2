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

# Tomar una muestra aleatoria de 8,000 casos
set.seed(123)
muestra_8000 <- Nacimientos_Bogota %>% sample_n(8000)

# Dividir la muestra en entrenamiento (80%) y prueba (20%)
n <- nrow(muestra_8000)
index <- sample(n, size = round(0.8 * n))

train <- muestra_8000[index, ]
test  <- muestra_8000[-index, ]

# Separar variables predictoras y variable objetivo
train_input <- train %>% select(Tiempo_gestacion, Tipo_parto, Numero_control_prenatal, Edad_madre, Numero_embarazos)
test_input  <- test %>% select(Tiempo_gestacion, Tipo_parto, Numero_control_prenatal, Edad_madre, Numero_embarazos)

train_output <- as.factor(train$Peso)
test_output  <- as.factor(test$Peso)

# Asegurarse que las columnas sean numéricas
train_input[] <- lapply(train_input, as.numeric)
test_input[]  <- lapply(test_input, as.numeric)

# Aplicar kNN (usa k impar para evitar empates)
k <- 1:120
pred <- knn(train = train_input, test = test_input, cl = train_output, k = k)

# Evaluar el modelo
mean(pred == test_output)  # Precisión del modelo
table(Predicho = pred, Real = test_output)  # Matriz de confusión

# (Opcional) Visualizar resultados
resultados <- data.frame(Predicho = pred, Real = test_output)
head(resultados)


