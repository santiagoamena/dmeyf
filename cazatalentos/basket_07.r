#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 ) / 1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}



#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------

Estrategia_A  <- function()
{
  #Estrategia
  #En la primer ronda se hace tirar 90 tiros libres a cada uno de los 100 jugadores ( se gastan 9000 tiros )
  #Se eligen a la mejor mitad de primer ronda( se descarta a la otra mitad)
  #En la segunda ronda, a la mejor mitad de la primera se los hace tirar 400 tiros a cada uno
  #Se elige el mejor jugador de la segunda ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   90  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

  planilla_cazatalentos[ ids_juegan1,  tiros1 := 90 ]  #registro en la planilla que tiran 90 tiros

  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, 90)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla

  #Ronda 2 -------------------------------------------------------
  #A la mitad mejor la hago tirar 400 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(aciertos1) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 >= mediana, id ]

  planilla_cazatalentos[ ids_juegan2,  tiros2 := 400 ]  #registro en la planilla que tiran 400 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, 400)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla

  #Ronda 3 -------------------------------------------------------
  #A la mitad mejor la hago tirar 400 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan2, median(aciertos2) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ aciertos2 >= mediana, id ]

  planilla_cazatalentos[ ids_juegan3,  tiros3 := 200 ]  #registro en la planilla que tiran 400 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, 200)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla


  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos3) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 102191 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_A()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total
tasa_eleccion_correcta

#Esta estrategia elije al verdadero_mejor el 99% de las veces
#pero lamentablemente necesita de un total de 36600   tiros libres












gimnasio_init()

#Esta el la planilla del cazatalentos
#el id es el numero que tiene en la espalda cada jugador
planilla_cazatalentos  <- data.table( "id"= 1:100 )

#Ronda 1  ------------------------------------------------------
#tiran los 100 jugadores es decir 1:100   90  tiros libres cada uno

ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

planilla_cazatalentos[ ids_juegan1,  tiros1 := 10 ]  #registro en la planilla que tiran 90 tiros

100*100 + 50*50 + 26*50 + 14*50 + 2*50
primero_ganador = 0
for (j in 1:10000) {
  lista = GLOBAL_jugadores
  lista = split(lista, ceiling(seq_along(lista)/2))
  lista = as.data.frame(lista)
  lista = as.data.frame(transpose(lista))
  pasan = c()
  for (i in 1:50) {
    a = ftirar(lista[i,'V1'], 50)
    b = ftirar(lista[i,'V2'], 50)
    if (a > b) pasan[i] = lista[i,'V1'] else pasan[i] = lista[i,'V2']
  }

  lista = split(pasan, ceiling(seq_along(pasan)/2))
  lista = as.data.frame(lista)
  lista = as.data.frame(transpose(lista))
  pasan2 = c()
  for (i in 1:25) {
    a = ftirar(lista[i,'V1'], 50)
    b = ftirar(lista[i,'V2'], 50)
    if (a > b) pasan2[i] = lista[i,'V1'] else pasan2[i] = lista[i,'V2']
  }

  lista = split(pasan2, ceiling(seq_along(pasan2)/2))
  lista = as.data.frame(lista)
  lista = as.data.frame(transpose(lista))
  pasan3 = c()
  for (i in 1:13) {
    a = ftirar(lista[i,'V1'], 50)
    b = ftirar(lista[i,'V2'], 50)
    if (a > b) pasan3[i] = lista[i,'V1'] else pasan3[i] = lista[i,'V2']
  }

  lista = split(pasan3, ceiling(seq_along(pasan3)/2))
  lista = as.data.frame(lista)
  lista = as.data.frame(transpose(lista))
  pasan4 = c()
  for (i in 1:7) {
    a = ftirar(lista[i,'V1'], 50)
    b = ftirar(lista[i,'V2'], 50)
    if (a > b) pasan4[i] = lista[i,'V1'] else pasan4[i] = lista[i,'V2']
  }

  lista = split(pasan4, ceiling(seq_along(pasan4)/2))
  lista = as.data.frame(lista)
  lista = as.data.frame(transpose(lista))
  pasan5 = c()
  for (i in 1:4) {
    a = ftirar(lista[i,'V1'], 50)
    b = ftirar(lista[i,'V2'], 50)
    if (a > b) pasan5[i] = lista[i,'V1'] else pasan5[i] = lista[i,'V2']
  }

  lista = split(pasan5, ceiling(seq_along(pasan5)/2))
  lista = as.data.frame(lista)
  lista = as.data.frame(transpose(lista))
  pasan6 = c()
  for (i in 1:2) {
    a = ftirar(lista[i,'V1'], 50)
    b = ftirar(lista[i,'V2'], 50)
    if (a > b) pasan6[i] = lista[i,'V1'] else pasan6[i] = lista[i,'V2']
  }

  if (pasan6[1] > pasan6[2]) mejor = pasan6[1] else mejor = pasan6[2]
  if (mejor == 0.7) primero_ganador = primero_ganador + 1
}
primero_ganador / 10000

#Hago que tiren
resultado1  <- gimnasio_tirar( ids_juegan1, 90)
planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla

#Ronda 2 -------------------------------------------------------
#A la mitad mejor la hago tirar 400 tiros cada uno
#La mediana siempre parte a un conjunto en dos partes de igual cantidad
mediana  <- planilla_cazatalentos[ ids_juegan1, median(aciertos1) ]
ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 >= mediana, id ]

planilla_cazatalentos[ ids_juegan2,  tiros2 := 200 ]  #registro en la planilla que tiran 400 tiros
resultado2  <- gimnasio_tirar( ids_juegan2, 200)
planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla


#Ronda 3 -------------------------------------------------------
#A la mitad mejor la hago tirar 400 tiros cada uno
#La mediana siempre parte a un conjunto en dos partes de igual cantidad
mediana  <- planilla_cazatalentos[ ids_juegan2, median(aciertos2) ]
ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ aciertos2 >= mediana, id ]

planilla_cazatalentos[ ids_juegan3,  tiros3 := 200 ]  #registro en la planilla que tiran 400 tiros
resultado3  <- gimnasio_tirar( ids_juegan3, 200)
planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla


#Epilogo
#El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos3) ]
id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

#Finalmente, la hora de la verdadero_mejor
#Termino el juego
veredicto  <- gimnasio_veredicto( id_mejor )

return( veredicto )