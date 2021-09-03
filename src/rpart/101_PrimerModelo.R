rm(list=ls())
#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")
library(randomForest)
library(mlr)
library(ggplot2)
library(tidyverse)

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/santi/projects/maestria/dmef")  #Establezco el Working Directory

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasets_ori/paquete_premium_202009.csv")

dtrain$clase_ternaria[dtrain$clase_ternaria == "CONTINUA"] = 0
dtrain$clase_ternaria[dtrain$clase_ternaria == "BAJA+1"] = 0
dtrain$clase_ternaria[dtrain$clase_ternaria == "BAJA+2"] = 1

dtrain$clase_ternaria = as.factor(dtrain$clase_ternaria)

#genero el modelo
modelo  <- rpart("clase_ternaria ~ .",
                 data = dtrain,
                 xval=0,
                 cp=-0.3, 
                 minsplit=80,
                 minbucket=1,
                 maxdepth=8)

dt = sort(sample(nrow(dtrain), nrow(dtrain)*.7))
train<-as.data.frame(dtrain[dt,]) 
test<-as.data.frame(dtrain[-dt,])


dt_prob <- makeLearner('classif.rpart', predict.type="prob")
dt_task <- makeClassifTask(data=train, target="clase_ternaria")

getParamSet("classif.rpart")

dt_param <- makeParamSet( 
  makeDiscreteParam("minsplit", values=seq(60,100,10)), 
  makeDiscreteParam("minbucket", values=seq(1,5,1)), 
  makeNumericParam("cp", lower = -0.4, upper = 0.1), 
  makeDiscreteParam("maxcompete", values=6), 
  makeDiscreteParam("usesurrogate", values=0), 
  makeDiscreteParam("maxdepth", values=seq(6,10,1)) 
)


ctrl = makeTuneControlGrid()
rdesc = makeResampleDesc("CV", iters = 5L, stratify=TRUE)


dt_tuneparam <- tuneParams(learner=dt_prob, 
                          resampling=rdesc, 
                          measures=list(tpr,auc, fnr, mmce, tnr, setAggregation(tpr, test.sd)), 
                          par.set=dt_param, 
                          control=ctrl, 
                          task=dt_task, 
                          show.info = TRUE)

list('Optimal HyperParameters' = dt_tuneparam$x, 
     'Optimal Metrics' = dt_tuneparam$y)

dtree <- setHyperPars(dt_prob, par.vals = dt_tuneparam$x)
dtree_train <- train(learner=dtree, task=dt_task) 
getLearnerModel(dtree_train)
dtree_predict <- predict(dtree_train, newdata = test)

Performance <- performance(dtree_predict, measures = list(tpr,auc,mmce, acc,tnr)) %>% 
  as.data.frame(row.names = c("True Positive","Area Under Curve", "Mean Misclassification Error","Accuracy","True Negative")) Performance %>% kable(caption="Performance of Decision Tree",digits = 2, format = 'html', col.names = "Result")

dtree_threshold <- generateThreshVsPerfData(
  dtree_predict, 
  measures = list(tpr,auc, mmce,tnr)
  ) %>% plotThreshVsPerf() + geom_point() 

d = as.data.frame(dtree_predict)
unique(d$response)

dtrain$Master_delinquency[is.na(dtrain$Master_delinquency)] = 2
dtrain$Visa_delinquency[is.na(dtrain$Visa_delinquency)] = 2
dtrain$Master_status[is.na(dtrain$Master_status)] = 11
dtrain$Visa_status[is.na(dtrain$Visa_status)] = 11
                   
dtrain[is.na(dtrain)] = 0   

dtrain$clase_ternaria = as.numeric(dtrain$clase_ternaria)

modelo = randomForest(clase_ternaria ~ .,
                      data = dtrain)

#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasets_ori/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, 2] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K101_001.csv", sep="," )
