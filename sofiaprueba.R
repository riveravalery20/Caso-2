library(tidyverse)
library(caret)
library(pROC)

index_muestra <- sample(11873,11873)

index_entrena <- sample(11873,8904)

index_test <- index_muestra[!index_muestra %in% index_entrena]

BD_entrena <- Base_datos[index_entrena, ] %>% 
  filter(!is.na(Peso))

BD_test <- Base_datos[index_test, ]

BD_entrena_input <- BD_entrena[, 1:5]
BD_entrena_output <- BD_entrena[, 6]

BD_test_input <- BD_test[, -6]
BD_test_output <- BD_test[, 6]


fit_logit <- glm(Peso ~ Tiempo_gestación 
                 + Tipo_parto + Numero_control_prenatal 
                 + Edad_madre + Numero_embarazos, data = BD_entrena, family = binomial())
summary(fit_logit)  # Rmarkdown

p_hat <- predict(fit_logit, newdata = BD_test, type = "response")  # prob( Si )

pred_clase <- factor(ifelse(p_hat >= 0.5, "Si", "No"), levels = c("Si","No"))

confusionMatrix(pred_clase, BD_test$Peso, positive = "Si")

roc_o <- roc(response = BD_test$Peso, predictor = p_hat, levels = c("No","Si"))
thr   <- coords(roc_o, x = "best", best.method = "youden", ret = "threshold")


umbral<-as.numeric(thr)

# Recalculamos las predicciones usando el nuevo umbral óptimo.
pred_clase <- factor(ifelse(p_hat >= umbral, "Si", "No"), levels = c("Si","No"))

# Nueva matriz de confusión con el umbral ajustado.
confusionMatrix(pred_clase, test$admit_f, positive = "Si")



# ========= 4) ROC =========NJ

# La curva ROC (Receiver Operating Characteristic) muestra
# la relación entre la tasa de verdaderos positivos (sensibilidad)
# y la tasa de falsos positivos (1 - especificidad).
# El área bajo la curva (AUC) mide qué tan bien el modelo distingue
# entre admitidos y no admitidos. Cuanto más alto el AUC, mejor el modelo.

auc_val <- auc(roc_o)

plot(roc_o, main = sprintf("ROC Logit | AUC=%.3f | Umbral=%.3f", auc_val, thr))





# Interpretaciones



# - No Information Rate (0.6869):
#   Es la precisión que lograríamos si el modelo fuera “perezoso”
#   y predijera siempre la clase mayoritaria. En este caso,
#   la mayoría de los estudiantes NO son admitidos.
#   Por tanto, si el modelo dijera “No admitido” para todos,
#   acertaría el 68.7% de las veces.
#   Nuestro modelo tiene una precisión de 72.7%, un poco mejor que eso.

# - P-Value [Acc > NIR] (0.22):
#   Evalúa si esa mejora del 72.7% frente al 68.7% es estadísticamente significativa.
#   Como el valor p es mayor que 0.05, no podemos afirmar que el modelo
#   sea significativamente mejor que simplemente decir “No admitido” para todos.

# - Kappa (0.28):
#   Mide el nivel de acuerdo entre las predicciones del modelo y los valores reales,
#   corrigiendo el azar. Un valor de 0.28 indica un acuerdo bajo a moderado.
#   En palabras simples: el modelo acierta algo más de lo que esperaría por casualidad,
#   pero todavía no es muy confiable para decidir admisiones reales.

# - McNemar's p-value (0.02):
#   Evalúa si el modelo tiende a equivocarse más con una clase que con la otra.
#   Como el valor p es menor que 0.05, el modelo muestra un sesgo:
#   predice con mucha más seguridad los “No admitidos” que los “Admitidos”.
#   Es decir, tiene dificultades para reconocer a los estudiantes que sí fueron aceptados.

# ----------------------------------------------------------
#  Indicadores principales
# ----------------------------------------------------------

# - Sensibilidad (0.35):
#   De todos los estudiantes que SÍ fueron admitidos,
#   el modelo solo identificó correctamente el 35%.
#   Es decir, falla en muchos casos positivos (deja pasar varios admitidos reales).

# - Especificidad (0.90):
#   De todos los estudiantes que NO fueron admitidos,
#   el modelo acertó el 90%.
#   Es bastante bueno reconociendo a quienes no logran la admisión.

# - Valor predictivo positivo (0.61):
#   Cuando el modelo predice que alguien será admitido,
#   acierta el 61% de las veces.
#   Es decir, 6 de cada 10 estudiantes predichos como admitidos realmente lo son.

# - Valor predictivo negativo (0.75):
#   Cuando el modelo predice que un estudiante NO será admitido,
#   tiene razón el 75% de las veces.

# - Prevalencia (0.31):
#   Indica que el 31% de los estudiantes en el conjunto de prueba
#   fueron realmente admitidos (es decir, los casos positivos son minoría).

# - Balanced Accuracy (0.63):
#   Promedia la sensibilidad (35%) y la especificidad (90%),
#   para obtener una medida más justa del rendimiento global.
#   En este caso, el modelo tiene un desempeño “aceptable” (63%),
#   aunque sigue siendo más fuerte para reconocer “No admitidos”
#   que para detectar a los “Sí admitidos”.






###################### AUC y ROC



# - La curva ROC (Receiver Operating Characteristic) sirve para evaluar
#   qué tan bien el modelo logra separar a los "admitidos" de los "no admitidos"
#   cuando cambiamos el umbral que decide entre “Sí” o “No”.
#
#   Por ejemplo:
#   si el umbral es muy bajo (como 0.2), casi todos serán clasificados como “Sí”,
#   aumentando la sensibilidad pero también los falsos positivos.
#   Si el umbral es muy alto (como 0.8), casi todos serán “No”,
#   bajando los falsos positivos pero también la sensibilidad.
#
#   La curva ROC muestra todos esos escenarios posibles:
#   - En el eje Y: la sensibilidad (qué tanto detecta a los admitidos reales).
#   - En el eje X: 1 - especificidad (qué tanto se equivoca con los no admitidos).
#
#   Un modelo bueno tendrá una curva más “levantada” hacia la esquina superior izquierda.
#   Un modelo que adivina al azar quedaría como una diagonal (línea de 45°).
#
# - El AUC (Área Under the Curve) mide el área bajo esa curva ROC.
#   Es un número entre 0 y 1 que resume cuán bien el modelo distingue
#   entre admitidos y no admitidos sin depender de un umbral específico.
#
#   Interpretación:
#   - AUC = 1.0 → modelo perfecto, separa completamente a ambos grupos.
#   - AUC = 0.5 → modelo sin poder predictivo (equivale a lanzar una moneda).
#   - AUC = 0.67 (nuestro caso) → el modelo tiene una capacidad moderada
#     para distinguir entre admitidos y no admitidos.
#
#   En palabras simples:
#   si escogemos al azar un estudiante admitido y uno no admitido,
#   el modelo tiene un 67% de probabilidad de asignar una probabilidad
#   más alta de admisión al estudiante que realmente fue admitido.
#
#   Así, el AUC nos da una visión general del rendimiento del modelo,
#   más allá de un punto de corte específico como 0.5 o el umbral óptimo. 