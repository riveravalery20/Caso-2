# Librerías esenciales
library(dplyr)
library(tidyverse)
library(caret)
library(pROC)
library(class)

# KNN COMPLETO
set.seed(200)

# División de datos
index_muestra <- sample(11873, 11873)
index_entrena <- sample(11873, 8904)
index_test <- index_muestra[!index_muestra %in% index_entrena]

BD_entrena <- Base_datos[index_entrena, ] %>% 
  filter(!is.na(Peso_delicado))

BD_test <- Base_datos[index_test, ] %>% 
  filter(!is.na(Peso_delicado))

# Asegurar niveles correctos
BD_entrena$Peso_delicado <- factor(BD_entrena$Peso_delicado, levels = c("Si", "No"))
BD_test$Peso_delicado <- factor(BD_test$Peso_delicado, levels = c("Si", "No"))

BD_entrena_input <- Base_datos[, 1:5]
BD_entrena_output <- Base_datos[, 6]

BD_test_input <- Base_datos[, -6]
BD_test_output <- Base_datos[, 6]

# Entrenar k-NN
BD_knnEntrenado <- train(Peso_delicado ~ ., 
                         data = BD_entrena, 
                         method = "knn",  
                         tuneLength = 200
)
#Numero optimo

BD_test_output_kNN <- knn(train = BD_entrena_input, 
                            cl = BD_entrena_output, 
                            test = BD_test_input, 
                            k = 43)



# Resultados del modelo
BD_knnEntrenado

# Gráfica del k óptimo
plot(BD_knnEntrenado)

# Predicciones
BD_knnPrediccion <- predict(BD_knnEntrenado, newdata = BD_test)

# Matriz de confusión final
confusionMatrix(BD_knnPrediccion, BD_test$Peso_delicado, positive = "Si")

prob_knnPrediccion <- predict(BD_knnEntrenado, newdata = BD_test, type = "prob")
head(prob_knnPrediccion, 10)

data.frame(
  Real = BD_test$Peso_delicado[1:20],
  Predicho = BD_knnPrediccion[1:20],
  Prob_Si = round(prob_knnPrediccion$Si[1:20], 4),
  Prob_No = round(prob_knnPrediccion$No[1:20], 4)
)

roc_obj <- roc(BD_test$Peso_delicado, prob_knnPrediccion$Si)
auc_val <- auc(roc_obj)
plot(roc_obj, main = paste("Curva ROC k-NN - AUC =", round(auc_val, 3)))
legend("bottomright", legend = paste("AUC =", round(auc_val, 3)), bty = "n")
