#Librerias

library(readr)
library(dplyr)
library(tidyverse)
library(caret)
library(ISLR)
library(ROCR)
library(pROC)
#Base general

BD_EEVV <- read_csv("BD-EEVV-Nacimientos-2024.csv") %>%
  select(COD_DPTO, PESO_NAC, T_GES, TIPO_PARTO, NUMCONSUL, EDAD_MADRE, N_EMB, MES) %>%
  na.omit() %>% 
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
    Numero_control_prenatal = as.numeric(Numero_control_prenatal), 
    Numero_embarazos = as.numeric(Numero_embarazos),
    Mes = as.numeric(Mes))%>% 
  mutate(
    Peso = ifelse(Peso %in% c(5, 6, 7, 8, 9), "No", "Si"),
    Peso = factor(Peso, levels = c("Si", "No"))
  )%>%
  filter(Mes %in% c(7, 8, 9, 10, 11, 12)) %>% 
  filter(Departamento == 11) %>%
  mutate(Departamento = "Bogotá")

str(BD_EEVV)



#Balanceo
set.seed(5)

Si <- BD_EEVV %>% filter(Peso == "Si")
No <- BD_EEVV %>% filter(Peso == "No") %>% 
  sample_n(6000)

#Base final

Base_datos <- bind_rows(Si, No) %>% 
  select(Tiempo_gestación,Tipo_parto,Numero_control_prenatal,Edad_madre,Numero_embarazos,
         Peso) %>% mutate(Tipo_parto=case_when(
           Tipo_parto == 1 ~ "Espontaneo",
           Tipo_parto == 2 ~ "Cesarea",
           Tipo_parto == 3 ~ "Instrumentado"
         )) %>% 
  mutate(Edad_madre=case_when(
    Edad_madre %in% c(1, 2) ~ "Adolescente",
    Edad_madre %in% c(3, 4) ~ "Joven",
    Edad_madre %in% c(5, 6) ~ "Adulta",
    Edad_madre %in% c(7, 8, 9) ~ "Adulta mayor"),
    Tipo_parto = as.factor(Tipo_parto),
    Edad_madre = as.factor(Edad_madre)
  ) %>% 
  filter(complete.cases(.))

#KNN

index_muestra <- sample(11873,11873)

index_entrena <- sample(11873,8904)

index_test <- index_muestra[!index_muestra %in% index_entrena]

BD_entrena <- Base_datos[index_entrena, ] %>% 
  filter(!is.na(Peso))

BD_test <- Base_datos[index_test, ] %>% 
  filter(!is.na(Peso))

BD_entrena_input <- BD_entrena[, 1:5]
BD_entrena_output <- BD_entrena[, 6]

BD_test_input <- BD_test[, -6]
BD_test_output <- BD_test[, 6]

#K optimo

BD_knnEntrenado <- train(Peso ~ ., 
                         data = BD_entrena, 
                         method = "knn",  
                         tuneLength = 200
)


#K optimo 37
BD_knnEntrenado

plot(BD_knnEntrenado)

BD_knnPrediccion <- predict(BD_knnEntrenado, newdata = BD_test )

prob_knnPrediccion <- predict(BD_knnEntrenado, newdata = BD_test, type = "prob")

#Matriz de confusion

confusionMatrix(BD_knnPrediccion, BD_test$Peso)


## LOGIT
str(BD_entrena)
str(Base_datos)
table(BD_entrena$Peso)
table(BD_test$Peso)


#lot
fit_logit <- glm(Peso ~ Tiempo_gestación+ Tipo_parto + Numero_control_prenatal +
                   Numero_embarazos+Edad_madre, data = BD_entrena, family = binomial())


summary(fit_logit)  # Rmarkdown

p_hat <- predict(fit_logit, newdata =BD_test, type = "response") # prob( Si )

pred_clase <- factor(ifelse(p_hat >= 0.5, "Si", "No"), 
                     levels = c("Si", "No"))

BD_test$Peso <- factor(BD_test$Peso, levels = c("Si", "No"))




confusionMatrix(pred_clase, BD_test$Peso, positive = "Si")

roc_o <- roc(response = BD_test$Peso, predictor = p_hat, levels = c("No","Si"))
thr   <- coords(roc_o, x = "best", best.method = "youden", ret = "threshold")
auc_val <- auc(roc_o)

plot(roc_o, main = sprintf("ROC Logit | AUC=%.3f | Umbral=%.3f", auc_val, thr))


umbral<-as.numeric(thr)

thresholds <- seq(0, 0.9, by = 0.1)
accuracies <- sapply(thresholds, function(thr) {
  pred <- factor(ifelse(p_hat >= thr, "Si", "No"), levels = c("Si","No"))
  cm <- confusionMatrix(pred, BD_test$Peso, positive = "Si")
  cm$overall["Accuracy"]
})

print(accuracies)

#Matriz de correlaciones



