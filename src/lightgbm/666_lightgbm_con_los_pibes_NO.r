#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

setwd("C:/Users/santi/projects/maestria/dmef" )  #establezco la carpeta donde voy a trabajar

#cargo el dataset
dataset  <- fread("./datasets_ori/paquete_premium_202009.csv")

dataset$rentabilidad_prom = dataset$mrentabilidad_annual / dataset$cliente_antiguedad
dataset$mcomisiones_prom = dataset$mrentabilidad_annual / dataset$cliente_antiguedad
dataset$mpasivos_margen_t = sqrt(dataset$mpasivos_margen**2)
dataset$mrentabilidad_t = sqrt(dataset$mrentabilidad**2)
dataset$mrentabilidad_annual_t = sqrt(dataset$mrentabilidad_annual**2)
dataset$mcomisiones_t = sqrt(dataset$mcomisiones**2)
dataset$mactivos_margen_t = sqrt(dataset$mactivos_margen**2)
dataset$rentabilidad_prom_t = sqrt(dataset$rentabilidad_prom**2)
dataset$mcomisiones_prom_t = sqrt(dataset$mcomisiones_prom**2)

dataset$Card_delinquency = dataset$Master_delinquency + dataset$Visa_delinquency
dataset$Card_status = dataset$Master_status + dataset$Visa_status
dataset$Card_mfinanciacion_limite = dataset$Master_mfinanciacion_limite + dataset$Visa_mfinanciacion_limite
dataset$Card_msaldototal = dataset$Master_msaldototal + dataset$Visa_msaldototal
dataset$Card_msaldopesos = dataset$Master_msaldopesos + dataset$Visa_msaldopesos
dataset$Card_msaldodolares = dataset$Master_msaldodolares + dataset$Visa_msaldodolares
dataset$Card_mconsumospesos = dataset$Master_mconsumospesos + dataset$Visa_mconsumospesos
dataset$Card_mconsumosdolares = dataset$Master_mconsumosdolares + dataset$Visa_mconsumosdolares
dataset$Card_mlimitecompra = dataset$Master_mlimitecompra + dataset$Visa_mlimitecompra
dataset$Card_madelantopesos = dataset$Master_madelantopesos + dataset$Visa_madelantopesos
dataset$Card_madelantodolares = dataset$Master_madelantodolares + dataset$Visa_madelantodolares
dataset$Card_mpagado = dataset$Master_mpagado + dataset$Visa_mpagado
dataset$Card_mpagospesos = dataset$Master_mpagospesos + dataset$Visa_mpagospesos
dataset$Card_mpagosdolares = dataset$Master_mpagosdolares + dataset$Visa_mpagosdolares
dataset$Card_mconsumototal = dataset$Master_mconsumototal + dataset$Visa_mconsumototal
dataset$Card_cconsumos = dataset$Master_cconsumos + dataset$Visa_cconsumos
dataset$Card_cadelantosefectivo = dataset$Master_cadelantosefectivo + dataset$Visa_cadelantosefectivo

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#Quito el Data Drifting de  "ccajas_transacciones"  "Master_mpagominimo"
campos_buenos  <- setdiff( colnames(dataset),
                           c("clase_ternaria", "clase01", 'internet','tpaquete1','tmobile_app','cmobile_app_trx',
                             'mtarjeta_visa_descuentos','mtarjeta_visa_descuentos',
                             'ctarjeta_visa_descuentos','ctarjeta_master_descuentos', 
                             'mtarjeta_master_descuentos') )

#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )

#Solo uso DOS hiperparametros,  max_bin  y min_data_in_leaf
#Dadme un punto de apoyo y movere el mundo, Arquimedes
modelo  <- lightgbm( data= dtrain,
                     params= list( objective= "binary",
                                   max_bin= 15,
                                   min_data_in_leaf= 4000,
                                   learning_rate= 0.05,
                                   num_iterations = 100 )  )


#cargo el dataset donde aplico el modelo
dapply  <- fread("./datasets_ori/paquete_premium_202011.csv")

dapply$rentabilidad_prom = dapply$mrentabilidad_annual / dapply$cliente_antiguedad
dapply$mcomisiones_prom = dapply$mrentabilidad_annual / dapply$cliente_antiguedad
dapply$mpasivos_margen_t = sqrt(dapply$mpasivos_margen**2)
dapply$mrentabilidad_t = sqrt(dapply$mrentabilidad**2)
dapply$mrentabilidad_annual_t = sqrt(dapply$mrentabilidad_annual**2)
dapply$mcomisiones_t = sqrt(dapply$mcomisiones**2)
dapply$mactivos_margen_t = sqrt(dapply$mactivos_margen**2)
dapply$rentabilidad_prom_t = sqrt(dapply$rentabilidad_prom**2)
dapply$mcomisiones_prom_t = sqrt(dapply$mcomisiones_prom**2)

dapply$Card_delinquency = dapply$Master_delinquency + dapply$Visa_delinquency
dapply$Card_status = dapply$Master_status + dapply$Visa_status
dapply$Card_mfinanciacion_limite = dapply$Master_mfinanciacion_limite + dapply$Visa_mfinanciacion_limite
dapply$Card_msaldototal = dapply$Master_msaldototal + dapply$Visa_msaldototal
dapply$Card_msaldopesos = dapply$Master_msaldopesos + dapply$Visa_msaldopesos
dapply$Card_msaldodolares = dapply$Master_msaldodolares + dapply$Visa_msaldodolares
dapply$Card_mconsumospesos = dapply$Master_mconsumospesos + dapply$Visa_mconsumospesos
dapply$Card_mconsumosdolares = dapply$Master_mconsumosdolares + dapply$Visa_mconsumosdolares
dapply$Card_mlimitecompra = dapply$Master_mlimitecompra + dapply$Visa_mlimitecompra
dapply$Card_madelantopesos = dapply$Master_madelantopesos + dapply$Visa_madelantopesos
dapply$Card_madelantodolares = dapply$Master_madelantodolares + dapply$Visa_madelantodolares
dapply$Card_mpagado = dapply$Master_mpagado + dapply$Visa_mpagado
dapply$Card_mpagospesos = dapply$Master_mpagospesos + dapply$Visa_mpagospesos
dapply$Card_mpagosdolares = dapply$Master_mpagosdolares + dapply$Visa_mpagosdolares
dapply$Card_mconsumototal = dapply$Master_mconsumototal + dapply$Visa_mconsumototal
dapply$Card_cconsumos = dapply$Master_cconsumos + dapply$Visa_cconsumos
dapply$Card_cadelantosefectivo = dapply$Master_cadelantosefectivo + dapply$Visa_cadelantosefectivo

#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))

#la probabilidad de corte ya no es 0.025,  sino que 0.031
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.031) ) ) #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm.csv",
        sep=  "," )
