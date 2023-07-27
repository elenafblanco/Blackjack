# Esperanza matemática en función de un valor fijo de parada del jugador.

# Borrar memoria del espacio de trabajo (reestablecer mazo)
  rm(list = ls())

# Definimos los parámetros del juego
  pago_total <- 0
  simulaciones_totales <- 10000
  num_barajas <- 6    # Número de barajas usadas
  valor_parada <- 17  # Valor de parada del jugador
  
  for (i in 1:simulaciones_totales) {
    # Barajamos las cartas
    cartas <- c(rep(2:9, 4), rep(10, 16), rep(11,4))
    baraja <- rep(cartas, num_barajas)
    baraja <- sample(baraja)
    
    # Repartimos las cartas del jugador y del crupier
    mano_jugador <- sample(baraja, 2)
    baraja <- baraja[-which(baraja %in% mano_jugador)]
    mano_crupier <- sample(baraja, 2)
    baraja <- baraja[-which(baraja %in% mano_crupier)]
    
    # Turno del jugador
    while (sum(mano_jugador) < valor_parada) {
      siguiente_carta <- baraja[1]
      baraja <- baraja[-1]
      mano_jugador <- c(mano_jugador, siguiente_carta)
      jugada_2 <-  mano_jugador
      jugador_1 <- mano_jugador
      
      if (sum(mano_jugador == "11" & sum(mano_jugador) > 21 ) > 0) { 
        jugador_1 <- jugada_2 
        jugador_2 <- c(jugada_2[which(jugada_2 != "11")],1)  
        
        if (sum(mano_jugador == "11" & sum(mano_jugador) > 21) == 2) {
          jugador_3 <- c(jugador_2,1)
          jugador_2 <- jugador_3
        }
        
        if (sum(mano_jugador == "11" & sum(mano_jugador) >21) == 3) { 
          jugador_4 <- c(jugador_2,1,1)
          jugador_2 <- jugador_4
        }
        
        if (sum(mano_jugador == "11") == 4 & sum(mano_jugador) >21) { 
          jugador_5 <- c(jugador_2,1,1,1)
          jugador_2 <- jugador_5
        }
    }
  }
    # Turno del crupier
    while (sum(mano_crupier) < 17) {
      siguiente_carta <- baraja[1]
      baraja <- baraja[-1]
      mano_crupier <- c(mano_crupier, siguiente_carta)
      jugada_crup <- mano_crupier
      total_crup <- sum(mano_crupier)
      
      if (sum(jugada_crup=="11") > 0 & sum(jugada_crup) > 21) {
        total_crup <- sum(jugada_crup)-10 
      }
      
    
    
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
  }
    
    # Calculamos el resultado y el pago
    suma_jugador <- sum(mano_jugador)
    suma_crupier <- sum(mano_crupier)
    pago <- 0
    
    if (suma_jugador > 21) {
      pago <- 1  
    } else if (suma_crupier > 21) {
      pago <- -1  
    } else if (suma_jugador > suma_crupier) {
      pago <- -1  
    } else if (suma_jugador < suma_crupier) {
      pago <- 1  
    } else {
      pago <- 0  
    }
    
    pago_total <- pago_total + pago
  }

  pago_esperado <- pago_total / simulaciones_totales
  
  print(paste("Esperanza matemática:", pago_esperado))

