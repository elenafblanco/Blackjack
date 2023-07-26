# Borrar memoria del espacio de trabajo (reestablecer mazo)
  rm(list = ls())

# Crear un mazo de cartas
  mazo <- c(rep(2:9, 4), rep(10, 16), rep(11,4))

# Mezclar el mazo
  mazo <- sample(mazo) 

# Repartir las dos primeras cartas al jugador
  jugador <- c(sample(mazo, 2)) 
  mazo <- mazo[-jugador]

# Mazo en el caso de que el jugador obtenga las dos cartas iguales
  if (length(unique(jugador))==1){
   mazo <- mazo[-jugador]
  }

# Repartir una carta al crupier
  crupier <- sample(mazo, 1) 
  mazo <- mazo[-crupier]


# Mostrar las cartas iniciales del jugador y la carta del crupier
  cat("Cartas del jugador:", paste(jugador, collapse = ", "), "\n")
  cat("Carta visible del crupier:", crupier, "\n")

# Ajustar el valor de la mano inicial del jugador en función del valor del as
  if (sum(jugador == "11") == 2 ){
    jugador <- c(11,1)
  } else { jugador <- sum(jugador)
  }

  valor_mano <- sum(jugador)

# Preguntar al jugador si quiere pedir una carta o plantarse
  jugada_2 <-  jugador
  jugador_1 <- jugador
  sum(jugador_1)

  while (sum(jugador_1) < 21) {
    respuesta <- readline("¿Quieres pedir una carta? (s/n): ")
  
    if (tolower(respuesta) == "s") {
      carta <- sample(mazo, 1)
      jugada <- c(jugada_2, carta)
      mazo <- mazo[-carta]
      jugada_2 <- jugada
    
      if (sum(jugador == "11") > 0) { 
        jugador_1 <- jugada_2 
        jugador_2 <- c(jugada_2[which(jugada_2 != "11")],1)  
      
        if (sum(jugador=="11") == 2) {
          jugador_3 <- c(jugador_2,1)
          jugador_2 <- jugador_3
        }
        
        if (sum(jugador=="11") == 3) { 
          jugador_4 <- c(jugador_2,1,1)
          jugador_2 <- jugador_4
        }
      
        if (sum(jugador=="11")==4) { 
          jugador_5 <- c(jugador_2,1,1,1)
          jugador_2 <- jugador_5
       }
        
        if (sum(jugador_1) > 21 ) {
         jugador_1 <- jugador_2
        }
      } else { jugador_1 <- jugada_2
     }
    
        cat("Carta pedida:", carta, "\n")
        cat("Cartas del jugador:", paste(jugada_2, collapse = ", "), "\n")
    
     if (sum(jugador_1) > 21) {
       cat("¡Has perdido!\n")
       break
      }
    } else if (tolower(respuesta) == "n") {
      break
   }
  }


# Preguntar al crupier si quiere pedir carta o plantarse
  crupier
  jugada_crup <- crupier
  total_crup <- sum(crupier)
  while (total_crup < 17) {
   respuesta <- readline("¿Quiere el crupier pedir una carta? (s/n): ")
  
    if (tolower(respuesta) == "s") {
      carta <- sample(mazo, 1)
      crupier <- c(jugada_crup, carta)
      mazo <- mazo[-carta]
      jugada_crup <- crupier
   
 
      cat("Carta pedida:", carta, "\n")
     cat("Cartas del crupier:", paste(jugada_crup, collapse = ", "), "\n")
    
    } else if (tolower(respuesta) == "n") {
        jugada_crup <- crupier
        break
     }
   total_crup <- sum(jugada_crup)
  }

# Si hay un as
  if (sum(jugada_crup=="11") > 0 & sum(jugada_crup) > 21) {
   total_crup <- sum(jugada_crup)-10 
  }

  total_crup
  while (total_crup < 17) {
   respuesta <- readline("¿Quiere el crupier pedir otra carta? (s/n): ")
  
    if (tolower(respuesta) == "s") {
      carta <- sample(mazo, 1)
      crupier <- c(jugada_crup, carta)
      mazo <- mazo[-carta]
      jugada_crup <- crupier
      total_crup <- sum(jugada_crup)
    
     if (sum(jugada_crup=="11") > 0 & sum(jugada_crup) > 21) {
       total_crup <- sum(jugada_crup)-10 
      
      }
       if (sum(jugada_crup=="11") == 2 & sum(jugada_crup) > 21) {
         total_crup <- sum(jugada_crup)-10 
          if (total_crup > 21) {
           total_crup2 <- total_crup - 10
           total_crup <- total_crup2
         }
       }
     if (sum(jugada_crup=="11") == 3 & sum(jugada_crup) > 21) {
        total_crup <- sum(jugada_crup)-10 
        if (total_crup > 21) {
         total_crup2 <- total_crup - 10
         total_crup <- total_crup2
         if (total_crup > 21) {
           total_crup3<- total_crup - 10
           total_crup <- total_crup3
          }
        }
      }
     if (sum(jugada_crup=="11") == 4 & sum(jugada_crup) > 21) {
        total_crup <- sum(jugada_crup)-10 
       if (total_crup > 21) {
          total_crup2 <- total_crup - 10
          total_crup <- total_crup2
          if (total_crup > 21) {
            total_crup3<- total_crup - 10
            total_crup <- total_crup3
          if (total_crup > 21) {
            total_crup4<- total_crup - 10
            total_crup <- total_crup4
           }
          }
        }
      }
      cat("Carta pedida:", carta, "\n")
      cat("Cartas del crupier:", paste(jugada_crup, collapse = ", "), "\n")
    
   } else if (tolower(respuesta) == "n") {
     break
   }
  }

# Sumas totales de las manos
  cat("Suma de la mano del jugador:", paste(sum(jugador_1), collapse = ", "), "\n")
  cat("Suma de la mano del crupier:", paste(sum(total_crup), collapse = ", "), "\n")


# Comparar las manos del jugador y el crupier y determinar el resultado
  if (sum(jugador_1) > 21) {
    cat("¡Has perdido!\n")
  } else if (total_crup > 21 & sum(jugador_1) <= 21) {
    cat("¡Has ganado!\n")
  } else if (sum(jugador_1) > total_crup & sum(jugador_1) <= 21) {
   cat("¡Has ganado!\n")
  } else if (sum(jugador_1) < total_crup & total_crup <= 21) {
    cat("¡Has perdido!\n")
  } else if (sum(jugador_1) == total_crup) {
    cat("¡Empate!\n")
  }
