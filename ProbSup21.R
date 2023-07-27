# Probabilidad de un jugador de superar 21 en función del número de cartas

# Borrar memoria del espacio de trabajo
  rm(list = ls())

  m <- 1 # Número de barajas

# Crear un mazo de cartas
  mazo <- c(rep(2:9, 4*m), rep(10, 16*m), rep(11,4*m))
  
# Ejemplo del funcionamiento del programa
  mano <- matrix(sample(mazo,3,replace=FALSE),nrow=1,ncol=3); mano
  
  rowSums(mano)
  
  c <- ifelse(rowSums(mano) > 21,"SE PASA", "No se pasa") ; c




# Con r cartas:
  n <- 10000 # Número de simulaciones
  r <- 5 #Número de cartas
  
  mano <- matrix(replicate(n,sample(mazo,r,replace=FALSE)),nrow=n,ncol=r); mano


se_pasa <- 0
for (i in 1:1000){

  jugada_2 <-  mano[i, ]
  jugador_1 <- mano[i, ] 
  sum(jugador_1)

  if (sum(mano[i, ] == "11") > 0) { 
    jugador_1 <- jugada_2 
    jugador_2 <- c(jugada_2[which(jugada_2 != "11")],1)  
  }
    
    if (sum(mano[i, ] =="11") == 2) {
      jugador_3 <- c(jugador_2,1)
      jugador_2 <- jugador_3
    }
    
    if (sum(mano[i, ] =="11") == 3) { 
      jugador_4 <- c(jugador_2,1,1)
      jugador_2 <- jugador_4
    
  } else { jugador_1 <- jugada_2
  }
  
  sum(jugador_1)
  if (sum(jugador_1)>21){
    se_pasa1 <- se_pasa + 1
    se_pasa <- se_pasa1
  }
}
se_pasa 
p <-se_pasa/1000

  cat("La probabilidad de superar 21 con n cartas es:", p, "\n")


