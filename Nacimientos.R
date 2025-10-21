library(readr)
BD_EEVV_Nacimientos_2024 <- read_csv("BD-EEVV-Nacimientos-2024.csv")
View(BD_EEVV_Nacimientos_2024)

Nacimientos=data.frame(BD_EEVV_Nacimientos_2024)

library(tidyverse)

Nacimientos=Nacimientos %>%select(PESO_NAC,T_GES,TIPO_PARTO,NUMCONSUL,EDAD_MADRE,N_EMB)
View(Nacimientos)

Nacimientos=Nacimientos %>%
  rename(
    Peso = PESO_NAC,
    Tiempo_gestación = T_GES,
    Tipo_parto= TIPO_PARTO,
    Numero_control_prenatal= NUMCONSUL,
    Edad_madre=EDAD_MADRE,
    Numero_embarazos=N_EMB
  )
Nacimientos=Nacimientos %>%
  mutate(
    Peso = ifelse(
      Peso %in% c("5","6", "7", "8","9"),
      "Moderado",
      "Delicado"
    )
  )
View(Nacimientos)

#Preparación del aprendizaje
library(tidyverse)

Nacimientos_modelo <- Nacimientos %>%
  mutate(
    Tipo_parto = as.factor(factor(Tipo_parto)),
    Tiempo_gestación = as.numeric(Tiempo_gestación),
    Numero_control_prenatal = as.numeric(Numero_control_prenatal),
    Edad_madre = as.factor(Edad_madre),
    Numero_embarazos = as.numeric(Numero_embarazos)
  )

input <- Nacimientos_modelo %>% select(-Peso)
output <- Nacimientos_modelo$Peso

set.seed(123)
index <- sample(nrow(Nacimientos_modelo), 0.7 * nrow(Nacimientos_modelo))
train_input <- input[index, ]
train_output <- output[index]
test_input <- input[-index, ]
test_output <- output[-index]

library(class)

pred <- knn(train = train_input, cl = train_output, test = test_input, k = 3)
mean(pred == test_output)  # Precisión
table(pred, test_output)   # Matriz de confusión







#Clase Joaqui
library(tidyverse)

set.seed(120)

index_muestra <- sample(453,901, 100)

index_entrena <- sample(index_muestra, 80)

index_test <- index_muestra[!index_muestra %in% index_entrena]


iris_entrena <- iris[index_entrena, ]
View(iris_entrena)
iris_test <- iris[index_test, ]
View(iris_test)

iris_entrena_input <- iris_entrena[, 1:4]
iris_entrena_output <- iris_entrena[, 5]

iris_test_input <- iris_test[, -5]
iris_test_output <- iris_test[, 5]


install.packages("class")
library(class)

iris_test_output_kNN <- knn(train = iris_entrena_input, 
                            cl = iris_entrena_output, 
                            test = iris_test_input, 
                            k = 3)

iris_test_output_kNN

mean(iris_test_output_kNN == iris_test_output)

table(iris_test_output_kNN, iris_test_output)

Nacimientos(iris_test_output_kNN, iris_test_output)


library(tidyverse)

k <- 1:50
resultado <- Nacimientos(k, precision = 0)

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

