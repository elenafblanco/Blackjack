# Borrar memoria del espacio de trabajo
  rm(list = ls())

# 1.- Probabilidad de un jugador de obtener blackjack con una baraja

# Creamos una baraja de cartas
  baraja <- c(rep(2:9, 4), rep(10, 16), rep(11,4))

# Mezclamos la baraja
  baraja <- sample(baraja)

# Simulamos n partidas distintas
  n <- 1000000
  mano <- matrix(replicate(n,sample(baraja,2,replace=FALSE)),nrow=n,ncol=2)
  rowSums(mano)
  c <- ifelse(rowSums(mano)==21,"Blackjack", "No blackjack")
  # El vector c contiene los resultados de blackjack o no blackjack 
  # correspondentes a n partidas

# Obtenemos la probabilidad de blackjack
  a=which(c=="Blackjack")
  as.vector(a)
  p=length(a)/n

cat("La probabilidad de blackajck es:", p)

# Vemos que el resultado concuerda con el obtenido en el Capítulo 2 del
# trabajo, en el apartado 2.1.1.
  

# 2.- Probabilidad de un jugador de obtener blackjack en función del número de barajas m

# Creamos m barajas de cartas
m=3
barajas <- c(rep(2:9, 4*m), rep(10, 16*m), rep(11,4*m))

# Mezclamos las barajas
barajas <- sample(barajas)

# Simulamos n partidas distintas
n=1000000
mano <- matrix(replicate(n,sample(barajas,2,replace=FALSE)),nrow=n,ncol=2)
rowSums(mano)
c=ifelse(rowSums(mano)==21,"Blackjack", "No blackjack")
# El vector c contiene los resultados de blackjack o no blackjack 
# correspondentes a n partidas

# Obtenemos la probabilidad de blackjack
a=which(c=="Blackjack")
as.vector(a)
p=length(a)/n 

cat("La probabilidad de blackajck es:", p)

# Vemos que los resultados al modificar los el valor de m se corresponden con los
# obtenidos en el Capítulo 2 del trabajo, en la Tabla 2.1.
  


