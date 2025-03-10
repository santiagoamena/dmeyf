#Necesita para correr en Google Cloud
#16 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#clase_binaria1   1={BAJA+2,BAJA+1}    0={CONTINUA}
#Optimizacion Bayesiana de hiperparametros de  lightgbm
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para kaggle

# WARNING  usted debe cambiar este script si lo corre en su propio Linux

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("lightgbm")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")
library('glue')

#para poder usarlo en la PC y en la nube sin tener que cambiar la ruta
#cambiar aqui las rutas en su maquina
switch ( Sys.info()[['sysname']],
         Windows = { directory.root  <-  "C:/Users/santi/projects/maestria/dmef" },   #Windows
         Darwin  = { directory.root  <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root  <-  "~/buckets/b1/" } #Google Cloud
       )
#defino la carpeta donde trabajo
setwd( directory.root )



kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "672_nuevo"
karch_generacion  <- "./datasets_ori/paquete_premium_202009.csv"
karch_aplicacion  <- "./datasets_ori/paquete_premium_202011.csv"
kBO_iter    <-  150   #cantidad de iteraciones de la Optimizacion Bayesiana

#Aqui se cargan los hiperparametros
hs <- makeParamSet( 
         makeNumericParam("learning_rate",    lower= 0.01 , upper=    0.1),
         makeNumericParam("feature_fraction", lower= 0.2  , upper=    1.0),
         makeIntegerParam("min_data_in_leaf", lower= 0    , upper= 8000),
         makeIntegerParam("num_leaves",       lower=16L   , upper= 1024L),
         makeNumericParam("prob_corte",       lower= 0.020, upper=    0.055)
        )

campos_malos = c(
    'internet','tpaquete1','tmobile_app','cmobile_app_trx',
    'mtarjeta_visa_descuentos','mtarjeta_visa_descuentos',
    'ctarjeta_visa_descuentos','ctarjeta_master_descuentos', 
    'mtarjeta_master_descuentos','numero_de_cliente','foto_mes',
    'ccajeros_propios_descuentos','mcajeros_propios_descuentos',
    'active_quarter','cliente_vip'
)

ksemilla_azar  <- 102191  #Aqui poner la propia semilla
#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )

  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento

  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly

  return( experimento_actual )
}
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )

  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )

    cat( linea, file=archivo )
  }

  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )

  cat( linea, file=archivo, append=TRUE )  #grabo al archivo

  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------

PROB_CORTE  <- 0.025

fganancia_logistic_lightgbm   <- function(probs, datos) 
{
  vlabels  <- getinfo(datos, "label")
  vpesos   <- getinfo(datos, "weight")

  #aqui esta el inmoral uso de los pesos para calcular la ganancia correcta
  gan  <- sum( (probs > PROB_CORTE  ) *
               ifelse( vlabels== 1 & vpesos > 1, 48750, -1250 ) )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales, la semilla del mal ...

EstimarGanancia_lightgbm  <- function( x )
{
  GLOBAL_iteracion  <<- GLOBAL_iteracion + 1

  gc()
  PROB_CORTE <<- x$prob_corte   #asigno la variable global

  kfolds  <- 5   # cantidad de folds para cross validation

  param_basicos  <- list( objective= "binary",
                          metric= "custom",
                          first_metric_only= TRUE,
                          boost_from_average= TRUE,
                          feature_pre_filter= FALSE,
                          verbosity= -100,
                          seed= 999983,
                          max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                          min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                          lambda_l1= 0.0,         #por ahora, lo dejo fijo
                          lambda_l2= 0.0,         #por ahora, lo dejo fijo
                          max_bin= 31,            #por ahora, lo dejo fijo
                          num_iterations= 9999,    #un numero muy grande, lo limita early_stopping_rounds
                          force_row_wise= TRUE    #para que los alumnos no se atemoricen con tantos warning
                        )

  #el parametro discolo, que depende de otro
  param_variable  <- list(  early_stopping_rounds= as.integer(50 + 5/x$learning_rate) )

  param_completo  <- c( param_basicos, param_variable, x )

  set.seed( 999983 )
  modelocv  <- lgb.cv( data= dtrain,
                       eval= fganancia_logistic_lightgbm,
                       stratified= TRUE, #sobre el cross validation
                       nfold= kfolds,    #folds del cross validation
                       param= param_completo,
                       verbose= -100
                      )


  ganancia  <- unlist(modelocv$record_evals$valid$ganancia$eval)[ modelocv$best_iter ]

  ganancia_normalizada  <-  ganancia* kfolds  
  attr(ganancia_normalizada ,"extras" )  <- list("num_iterations"= modelocv$best_iter)  #esta es la forma de devolver un parametro extra

  param_completo$num_iterations <- modelocv$best_iter  #asigno el mejor num_iterations
  param_completo["early_stopping_rounds"]  <- NULL

   #si tengo una ganancia superadora, genero el archivo para Kaggle
   if(  ganancia > GLOBAL_ganancia_max )
   {
     GLOBAL_ganancia_max  <<- ganancia  #asigno la nueva maxima ganancia a una variable GLOBAL, por eso el <<-

     set.seed(ksemilla_azar)

     modelo  <- lightgbm( data= dtrain,
                          param= param_completo,
                          verbose= -100
                        )

    #calculo la importancia de variables
    tb_importancia  <- lgb.importance( model= modelo )
    fwrite( tb_importancia, 
            file= paste0(kimp, "imp_", GLOBAL_iteracion, ".txt"),
            sep="\t" )

     prediccion  <- predict( modelo, data.matrix( dapply[  , campos_buenos, with=FALSE]) )

     Predicted  <- as.integer( prediccion > x$prob_corte )

     entrega  <- as.data.table( list( "numero_de_cliente"= dapply$numero_de_cliente, 
                                      "Predicted"= Predicted)  )

     #genero el archivo para Kaggle
     fwrite( entrega, 
             file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
             sep= "," )
   }

   #logueo 
   xx  <- param_completo
   xx$ganancia  <- ganancia_normalizada   #le agrego la ganancia
   loguear( xx,  arch= klog )

   return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_", kscript, ".RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_", kscript, ".txt" )
kimp        <- paste0("./work/E",  kexperimento, "_", kscript, "_" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_", kscript, "_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

#si ya existe el archivo log, traigo hasta donde llegue
if( file.exists(klog) )
{
 tabla_log  <- fread( klog)
 GLOBAL_iteracion  <- nrow( tabla_log ) -1
 GLOBAL_ganancia_max  <- tabla_log[ , max(ganancia) ]
}


#cargo el dataset donde voy a entrenar el modelo
dataset  <- fread(karch_generacion)

#creo la clase_binaria2   1={ BAJA+2,BAJA+1}  0={CONTINUA}
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

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
dataset$Card_mpagominimo = dataset$Master_mpagominimo + dataset$Visa_mpagominimo

vars_importantes = c(
    'ctrx_quarter',
    'cpayroll_trx',
    'mcuentas_saldo',
    'ctarjeta_visa_transacciones',
    'mactivos_margen_t',
    'mpasivos_margen_t',
    'mprestamos_personales',
    'Master_Fvencimiento',
    'Visa_Fvencimiento',
    'mpasivos_margen'
)
dataset = as.data.frame(dataset)
for (i in seq(length(vars_importantes))){
    for (j in seq(length(vars_importantes))){
        newcol = paste0('newvar_',i,'_',j)
        dataset[,newcol] = dataset[,vars_importantes[i]]*dataset[,vars_importantes[j]]
    }
}
dataset = as.data.table(dataset)
#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01", campos_malos) )

#dejo los datos en el formato que necesita LightGBM
#uso el weight como un truco ESPANTOSO para saber la clase real
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01,
                        weight=  dataset[ , ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)] )


#cargo los datos donde voy a aplicar el modelo
dapply  <- fread(karch_aplicacion, stringsAsFactors= TRUE) #leo los datos donde voy a aplicar el modelo

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
dapply$Card_mpagominimo = dapply$Master_mpagominimo + dapply$Visa_mpagominimo

dapply = as.data.frame(dapply)
for (i in seq(length(vars_importantes))){
    for (j in seq(length(vars_importantes))){
        newcol = paste0('newvar_',i,'_',j)
        dapply[,newcol] = dapply[,vars_importantes[i]]*dapply[,vars_importantes[j]]
    }
}
dapply = as.data.table(dapply)
#Aqui comienza la configuracion de la Bayesian Optimization


funcion_optimizar  <- EstimarGanancia_lightgbm   #la funcion que voy a maximizar

configureMlr( show.learner.output= FALSE)

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
              fn=       funcion_optimizar, #la funcion que voy a maximizar
              minimize= FALSE,   #estoy Maximizando la ganancia
              noisy=    TRUE,
              par.set=  hs,     #definido al comienzo del programa
              has.simple.signature = FALSE   #paso los parametros en una lista
             )

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)  #se graba cada 600 segundos
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )   #cantidad de iteraciones
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI() )

#establezco la funcion que busca el maximo
surr.km  <- makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner= surr.km, control= ctrl)
} else {
  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista
}
library('data.table')
df = fread("C:/Users/santi/projects/maestria/dmef/modelitos/E1047_modelitos.csv.gz")
str(df)

df = df[df$foto_mes == 202011,]
df = subset(df, select=c(numero_de_cliente, E1047_328))
str(df)

fwrite( df,
        paste0( "./kaggle/E1047.csv"),
        sep= "," )