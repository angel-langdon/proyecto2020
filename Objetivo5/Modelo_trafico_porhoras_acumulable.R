library(readr)
library(stringr)
library(ggplot2)
library(gridExtra)
require(MASS)
require(ISLR)
require(psych)
library(caret)

path = setwd('/Users/lisagilyarovskaya/Downloads/cadaHoraHistorico')
estaciones = list.files(path)

mifuncion <- function(b) {
  
  almacen = data.frame()
  tabla_final = data.frame()
  
  iter = 1
  cont = 1
  fila = 1
  diam = c()
  
  for (fichero in b) {
    fichero <- as.data.frame(read.csv(file = estaciones[cont], sep = ';', encoding = 'UTF8'))
    fichero = subset(fichero, str_sub(fichero$nombre,1,3) == 'Mol')
    fichero = fichero[c('des_tramo', 'lectura', 'calidad_ambiental')]
    if (cont == 1){
      fichero$acumulado = fichero$lectura
    }
    else{
      if ((cont != 1) & (str_sub(b[cont], 6, 7) == str_sub(b[cont-1], 6, 7))){
        fichero$acumulado = fichero$lectura + antiguo$acumulado
      }
      else {
        fichero$acumulado = fichero$lectura
      }
    }
    
  #  print(cont)
    tabla_final = rbind(tabla_final, fichero)
    cont = cont + 1
    antiguo = fichero
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

#Comprobamos que los porcentajes de cada categoría se mantienen en trainDatos
ttt = table(trainDatos$calidad_ambiental); ttt 
100*ttt/sum(ttt) 

#--------------------------------
# Ahora vamos a diseñar la forma de crear el conjunto de validación a partir del
# conjunto Training con Kfold. Queremos varios conjuntos training-validation (40 en total)
myTrainControl = trainControl(method = "repeatedcv",  # k-fold
                              number = 10,  # num folds, es el k
                              repeats = 40) # num veces a repetir CV

#las variables explicativas son lectura y acumulado, sin interacciones
modeloTR = train(calidad_ambiental ~ lectura + acumulado, data = trainDatos, method='lda', 
                 trControl = myTrainControl)
modeloTR
modeloTR$method
# El mejor modelo de todas las veces que ha hecho CV
modeloTR$finalModel 
#Coeficientes de las variables en FDL, las W
modeloTR$finalModel$scaling  
modeloTR$results 

# Calculamos la accuracy con los datos train
ajusteTR = predict(modeloTR)
confuTR = table("real" = trainDatos$calidad_ambiental, "predicho" = ajusteTR); confuTR
accuTR = sum(diag(confuTR))/sum(confuTR); accuTR

# Calculamos la accuracy con los datos test, aqui en teoria siempre irá peor
ajusteTest = predict(modeloTR, testDatos)
confuTest = table("real" = testDatos$calidad_ambiental, "predicho" = ajusteTest); confuTest
accuTest = sum(diag(confuTest))/sum(confuTest); accuTest








