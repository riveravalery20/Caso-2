set.seed(28)

Base_datosN=Base_datos %>% 
  select(-Tipo_parto, -Edad_madre)

View(Base_datosN)

index_muestra <- sample(13049, 10439)

index_entrena <- sample(index_muestra, 8351)

index_test <- index_muestra[!index_muestra %in% index_entrena]


iris_entrena <- Base_datosN[index_entrena, ]
View(iris_entrena)
iris_test <- Base_datosN[index_test, ]
View(iris_test)

iris_entrena_input <- iris_entrena[, 1:3]
iris_entrena_output <- iris_entrena[, 4]

iris_test_input <- iris_test[, -4]
iris_test_output <- iris_test[, 4]

library(class)

iris_test_output_kNN <- knn(train = iris_entrena_input, 
                                  cl = iris_entrena_output, 
                                  test = iris_test_input, 
                                  k = 3)

iris_test_output_kNN

mean(iris_test_output_kNN == iris_test_output)

table(iris_test_output_kNN, iris_test_output)

data.frame(iris_test_output_kNN, iris_test_output)


library(tidyverse)

k <- 1:50
resultado <- data.frame(k, precision = 0)

for(n in k){
  iris_test_output_kNN <- knn(train = iris_entrena_input, 
                              cl = iris_entrena_output, 
                              test = iris_test_input, 
                              k = n)
  
  resultado$precision[n] <- mean(iris_test_output_kNN == iris_test_output)
}

resultado

resultado %>% 
  ggplot() +
  aes(k, precision) +
  geom_line()

library(caret)

library(ISLR)
data(Smarket)

Smarket %>% 
  head(10)

Smarket <- Smarket %>% 
  rename(Direccion = Direction) %>% 
  mutate(Direccion = ifelse(Direccion == "Up", "Sube", "Baja")) %>% 
  mutate_at(c("Direccion"), ~as.factor(.))


set.seed(28)
indxEntrena <- createDataPartition(y = Smarket$Direccion, p = 0.75, list = FALSE)

SP_entrena <- Smarket[indxEntrena,]
SP_test <- Smarket[-indxEntrena,]


set.seed(28)

SP_knnEntrenado <- train(Direccion ~ ., 
                         data = SP_entrena, 
                         method = "knn",  
                         tuneLength = 200
)


SP_knnEntrenado

plot(SP_knnEntrenado)


SP_knnPrediccion <- predict(SP_knnEntrenado, newdata = SP_test )

prob_knnPrediccion <- predict(SP_knnEntrenado, newdata = SP_test, type = "prob")

prob_knnPrediccion %>% 
  head(10)


confusionMatrix(SP_knnPrediccion, SP_test$Direccion)




library(ROCR)

#Haciendo uso del paquete ROCR podemos graficar la curva ROC, 
#la cual nos da una idea de la calidad del modelo a partir de las 
#relaciones entre Falsos Positivos (False Positives) y 
#Verdaderos Positivos (True Positives) obtenidos sobre el conjunto de validación:

pred1 <- prediction(as.numeric(SP_knnPrediccion), as.numeric(SP_test$Direccion))
perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1)



#Como podemos ver, la línea resultante está bastante alejada de la diagonal que, 
#para este tipo de curvas, representa la selección completamente al azar de las 
#clases, lo que refleja entonces un buen desempeño del clasificador para este 
#conjunto de datos. A fin de comparar este con los demás clasificadores a 
#implementar, nos basaremos en el estudio de los resultados de clasificación
#con el conjunto de validación, así como la curva ROC.




#### Definiciones



# Accuracy (Exactitud): Es la proporción de predicciones correctas (tanto verdaderos positivos como verdaderos negativos) entre el total de casos examinados. Proporciona una medida general de la calidad del modelo.
# 
# 95% CI (Intervalo de Confianza del 95%): Es un rango de valores, derivado de las estadísticas de la muestra, que probablemente contenga el valor real de una población. El 95% indica que hay un 95% de confianza de que el rango contiene dicho valor verdadero.
# 
# No Information Rate (Tasa de No Información): Es una tasa de precisión que se podría obtener al hacer predicciones usando la clase más frecuente sin tener en cuenta las características. Es una línea base con la que se compara la exactitud del modelo. Por tanto, el NIR proporciona un punto de referencia básico; cualquier modelo que desarrolles debería ser capaz de superar esta tasa de acierto para ser considerado útil.
# 
# P-Value [Acc > NIR] (P-Valor [Exactitud > Tasa de No Información]): Es el valor p que prueba la hipótesis nula de que la exactitud del modelo no es mejor que la tasa de no información. Un valor p bajo sugiere que el modelo tiene una precisión significativamente mejor que la tasa de no información.
# 
# Kappa: Es una medida de acuerdo que corrige el acuerdo que se podría esperar por casualidad en la clasificación categórica. El valor de Kappa puede ser interpretado como qué tan mejor es el acuerdo entre las predicciones y los valores verdaderos en comparación con el acuerdo que se esperaría al azar.
# 
# Mcnemar's Test P-Value (Valor p del Test de McNemar): Es el valor p asociado con el Test de McNemar, que se utiliza para determinar si hay diferencias entre dos modelos de clasificación sobre los mismos casos.
# 
# Sensitivity (Sensibilidad): También conocida como el verdadero positivo, es la proporción de casos positivos reales que fueron identificados correctamente por el modelo.
# 
# Specificity (Especificidad): También conocida como el verdadero negativo, es la proporción de casos negativos reales que fueron identificados correctamente por el modelo.
# 
# Pos Pred Value (Valor Predictivo Positivo): Es la proporción de casos positivos predichos que son realmente positivos.
# 
# Neg Pred Value (Valor Predictivo Negativo): Es la proporción de casos negativos predichos que son realmente negativos.
# 
# Prevalence (Prevalencia): Es la proporción de casos positivos en el conjunto de datos.
# 
# Detection Rate (Tasa de Detección): Es la proporción de verdaderos positivos en el conjunto de datos.
# 
# Detection Prevalence (Prevalencia de Detección): Es la proporción de predicciones positivas en el conjunto de datos.
# 
# Balanced Accuracy (Exactitud Equilibrada): Es el promedio de la sensibilidad y la especificidad. Se utiliza cuando las clases están desbalanceadas, es decir, hay más casos en una clase que en la otra.


