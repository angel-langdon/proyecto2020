library(readr)
library(stringr)
library(ggplot2)
library(gridExtra)
require(MASS)
require(ISLR)
require(psych)
library(caret)
library(corrplot)

path = setwd('C:/Users/34601/Documents/UNI/2/PRY/diasHistoricos')
estaciones = list.files(path)

viento = as.data.frame(read.csv(file='C:/Users/34601/Documents/UNI/2/PRY/viento/vientoClasificiacion.csv', sep=',', encoding = 'UTF-8'))
str_sub(viento$velmedia, start = 2, end = 2, omit_na = FALSE) <- '.'
viento$velmedia = as.numeric(viento$velmedia)
mifuncion <- function(b) {
  
  almacen = data.frame()
  tabla_final = data.frame()
  cont = 1
  
  for (fichero in b) {
    fichero <- as.data.frame(read.csv(file = estaciones[cont], sep = ';', encoding = 'UTF8'))
    fichero = subset(fichero, str_sub(fichero$nombre,1,3) == 'Mol')
    fichero = fichero[c('des_tramo', 'lectura', 'calidad_ambiental')]
    fichero$media = fichero$lectura/24
    fichero$viento = viento$velmedia[[cont]]
    tabla_final = rbind(tabla_final, fichero)
    cont = cont + 1
  }
  return(tabla_final)
}

x = mifuncion(estaciones)
x


#Preproceso
#Miramos como son las porcentajes de cada factor de la variable calidad ambiental
newdata1 <- subset(x, x$calidad_ambiental != 'Error')
newdata1$calidad_ambiental = factor(newdata1$calidad_ambiental)
table(newdata1$calidad_ambiental)
100*table(newdata1$calidad_ambiental)/sum(table(newdata1$calidad_ambiental))

trainFilas = createDataPartition(newdata1$calidad_ambiental, p=0.8, list=FALSE)
head(trainFilas) 

#La separamos en datos de entrenamiento y de test
trainDatos = newdata1[trainFilas,] 
testDatos = newdata1[-trainFilas,]

#Comprobamos que los porcentajes de cada categor�a se mantienen en trainDatos
ttt = table(trainDatos$calidad_ambiental); ttt 
100*ttt/sum(ttt) 

#--------------------------------
# Ahora vamos a diseñar la forma de crear el conjunto de validación a partir del
# conjunto Training con Kfold. Queremos varios conjuntos training-validation (40 en total)
myTrainControl = trainControl(method = "repeatedcv",  # k-fold
                              number = 10,  # num folds, es el k
                              repeats = 40) # num veces a repetir CV

#las variables explicativas son lectura y acumulado, sin interacciones
modeloTR = train(calidad_ambiental ~ lectura + viento, data = trainDatos, method='lda', 
                 trControl = myTrainControl)
modeloTR
modeloTR$method
modeloTR$finalModel # el mejor modelo de todas las veces que ha hecho CV
modeloTR$finalModel$scaling  ## Coeficientes de las variables en FDL, las W
modeloTR$results 

# Calculamos la accuracy con los datos train
ajusteTR = predict(modeloTR)
confuTR = table("real" = trainDatos$calidad_ambiental, "predicho" = ajusteTR); confuTR
accuTR = sum(diag(confuTR))/sum(confuTR); accuTR

# Calculamos la accuracy con los datos test, aqui en teoria siempre irá peor
ajusteTest = predict(modeloTR, testDatos)
confuTest = table("real" = testDatos$calidad_ambiental, "predicho" = ajusteTest); confuTest
accuTest = sum(diag(confuTest))/sum(confuTest); accuTest



anova_viento = aov(viento ~ calidad_ambiental, data=newdata1)
summary(anova_viento)

anova_lectura = aov(lectura ~ calidad_ambiental, data=newdata1)
summary(anova_lectura)


