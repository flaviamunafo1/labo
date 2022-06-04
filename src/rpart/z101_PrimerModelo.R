#LIbrerias
install.packages("Matrix")
library(Matrix, lib.loc = "C:/Program Files/R/R-4.2.0/library")
install.packages("Hmisc")
library(Hmisc)
install.packages("rlist")
library(rlist)
install.packages("yaml")
library(yaml)
install.packages("primes")
library(primes)
install.packages("bit64")
library(bit64)
install.packages("IRdisplay")
library(IRdisplay)
library(repr)
install.packages("vioplot")
library(vioplot)
install.packages("DT")
library(DT)
install.packages("ROCR")
library(ROCR)
install.packages("R.utils")
library(R.utils)
install.packages("Rcpp")
library(Rcpp)
install.packages("devtools")
library(devtools)
library(ggplot2)
install.packages("gganimate")
library(gganimate)
install.packages("transformr")
library(transformr)
install.packages("DiagrammeR")
library(DiagrammeR)
install.packages("data.table")
library(data.table)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("treeClust")
library(treeClust)
install.packages("ranger")
library(ranger)
install.packages("randomForest")
library(randomForest)
install.packages("xgboost")
library(xgboost)
install.packages("lightgbm")
library(lightgbm)
install.packages("DiceKriging")
library(DiceKriging)
install.packages("mlrMBO")
library(mlrMBO)

#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart   y rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de SU computadora local
setwd("/Users/flaviamunafo/Desktop/mineriadedatos/labo")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("/Users/flaviamunafo/Desktop/mineriadedatos/datasets/paquete_premium_202011.csv")

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
                 cp=        -0.1,   #esto significa no limitar la complejidad de los splits
                 minsplit=  90,     #minima cantidad de registros para que se haga el split
                 minbucket=  4,     #tamaño minimo de una hoja
                 maxdepth=   20)    #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("/Users/flaviamunafo/Desktop/mineriadedatos/datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "/Users/flaviamunafo/Desktop/mineriadedatos/labo/exp") 
dir.create( "/Users/flaviamunafo/Desktop/mineriadedatos/labo/exp/KA2004" ) 

fwrite( entrega, 
        file= "/Users/flaviamunafo/Desktop/mineriadedatos/labo/exp/KA2004/K200_001.csv", 
        sep= "," )
