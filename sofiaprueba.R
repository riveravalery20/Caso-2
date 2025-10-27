library(tidyverse)
library(caret)
library(pROC)

index_muestra <- sample(11873,11873)

index_entrena <- sample(11873,8904)

index_test <- index_muestra[!index_muestra %in% index_entrena]

BD_entrena <- Base_datos[index_entrena, ] %>% 
  filter(!is.na(Peso_delicado))

BD_test <- Base_datos[index_test, ]

BD_entrena_input <- BD_entrena[, 1:5]
BD_entrena_output <- BD_entrena[, 6]

BD_test_input <- BD_test[, -6]
BD_test_output <- BD_test[, 6]

BD_entrena$Peso <- factor(BD_entrena$Peso_delicado, levels = c("No", "Si"))
BD_test$Peso <- factor(BD_test$Peso_delicado, levels = c("No", "Si"))

fit_logit <- glm(Peso_delicado ~ Tiempo_gestación 
                 + Tipo_parto + Numero_control_prenatal 
                 + Edad_madre + Numero_embarazos, data = BD_entrena, family = binomial())
summary(fit_logit)  # Rmarkdown

p_hat <- predict(fit_logit, newdata = BD_test, type = "response")

pred_clase <- factor(ifelse(p_hat >= 0.5, "Si", "No"), levels = c("Si","No"))

confusionMatrix(pred_clase, BD_test$Peso_delicado, positive = "Si")

roc_o <- roc(response = BD_test$Peso_delicado, predictor = p_hat, levels = c("No","Si"))
thr   <- coords(roc_o, x = "best", best.method = "youden", ret = "threshold")

umbral<-as.numeric(thr)
pred_clase <- factor(ifelse(p_hat >= umbral, "Si", "No"), levels = c("Si","No"))

confusionMatrix(pred_clase, BD_test$Peso_delicado, positive = "Si")

auc_val <- auc(roc_o)

plot(roc_o, main = sprintf("ROC Logit | AUC=%.3f | Umbral=%.3f", auc_val, thr))



