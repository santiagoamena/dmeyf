#Necesita para correr en Google Cloud
#16 GB de memoria RAM
#256 GB de espacio en el disco local
#4 vCPU
# LightGBM  totalmente puro, con parametros por default, sin quitar ninguna variable

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

setwd("C:/Users/santi/projects/maestria/dmef")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets_ori/paquete_premium_202009.csv")

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param= list( objective= "binary")
                    )


#aplico el modelo a los datos sin clase
dapply  <- fread("./datasets_ori/paquete_premium_202011.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > 0.025)  ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm.csv", 
        sep= "," )
