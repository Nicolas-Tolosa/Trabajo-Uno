install.packages("tidyverse")
library(tidyverse)

rm(list = ls())

####################
### Ejercicio 1 ####
####################

# Funcion votaciones, recibe los parametros total; votosSI; votosNO, con la condicion de que votosSI + votosNO = total, debido al supuesto de que no hay votos blancos ni nulos
votaciones <- function(total,votosSI,votosNO){
  if (votosSI >= ((total/2)+1)) {
    print("Ha ganado el SI")
  } else{
    print("votosSI no tiene Quorum")
    if (votosSI >= total*0.3) {
      print("votosSI tiene un 30% de Quorum")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("Ha ganado el SI")
      } else{
        if (votosNO >= ((total/2)+1)) {
          print("Ha ganado el NO")
        } else{
          print("votosNO tiene un 30% de Quorum")
          if (votosSI == votosNO) {
            print("Los Quorums son identicos, ha ganado el NO debido al empate")
          } else{
            if (votosSI > votosNO) {
              print("Ha ganado el SI")
            } else{
              print("Ha ganado el NO")
            }
          }
        }
      }
    } else{
      print("Ha ganado el NO")
    }
  }
}

# Ejecuta la funcion, recibe los parametros total; votosSI; votosNO.
votaciones(10,6,4)


# Funcion votaciones_solo_total, recibe solamente el parametro "total" de votos, los votosSi y votosNO se calculan a traves de una seed de aleatoriedad dada por el total de votos (ejemplo: set.seed(total))
votaciones_solo_total <- function(total){
  set.seed(total)
  padron <- sample(c("SI","NO"),total,replace = TRUE)
  padron <- as.data.frame(padron)
  names(padron) <- c("votos")
  votosSI <- sum(with(padron,votos == "SI"))
  votosNO <- sum(with(padron,votos == "NO"))
  
  if (votosSI >= ((total/2)+1)) {
    print("Ha ganado el SI")
  } else{
    print("votosSI no tiene Quorum")
    if (votosSI >= total*0.3) {
      print("votosSI tiene un 30% de Quorum")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("Ha ganado el SI")
      } else{
        if (votosNO >= ((total/2)+1)) {
          print("Ha ganado el NO")
        } else{
          print("votosNO tiene un 30% de Quorum")
          if (votosSI == votosNO) {
            print("Los Quorums son identicos, ha ganado el NO debido al empate")
          } else{
            if (votosSI > votosNO) {
              print("Ha ganado el SI")
            } else{
              print("Ha ganado el NO")
            }
          }
        }
      }
    } else{
      print("Ha ganado el NO")
    }
  }
}

# Ejecuta la funcion, solo hay que ingresar el total de votos, 10 por defecto segun el enunciado.
votaciones_solo_total(10)

#La funcion set.seed() es una funcion util al momento de generar numeros aleatorios. En realidad no hay numeros aleatorios, sino pseudoaleatorios en la programacion. Estos numeros numeros son generados con un algoritmo, por lo que dicha generacion es almacenada en una semilla (seed), con el fin de predecir o reproducir la secuencia "aleatoria".

#La funcion samples() es una funcion que permite generar un vector con un cierto tipo de dato, con un tamaño de muestra definido, pudiendo reemplazar o no los datos aleatoriamente a traves de un vector de probabilidades o ejecutando set.seed() anteriormente.

####################
### Ejercicio 2 ####
####################



