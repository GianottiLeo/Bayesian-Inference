require(tinytex)
## Loading required package: tinytex
require(coda)
## Loading required package: coda
require(rmarkdown)
## Loading required package: rmarkdown
rm(list=ls(all=TRUE))
#### Declaracoes
v <- c(1,0,0.5,2,3,1,0.2,0.9,4,1,0,5) # Amostra para y definida de forma completamente arbitraria
y <- sample(v,4) # Escolha aleatória de um valor para os y
start <- c(1) # Definido de forma completamentearbitraria
a <- c(1000,900,700,850) # Amostra para M definida de forma completamente arbitraria
M <- sample(a,1) # Escolha aleatória de um valor para M 
n <- M
burn_in <- 150 # Definido de forma completamente arbitraria
thin <- 6 # Definido de forma completamente 
arbitraria
taxa=0
#### Distribuicao conjunta (x|y)
p=function(x,y){
  
  p=(2+x)^y[1]*(1-x)^(y[2]+y[3])*x^y[4] 
  
  return(p)
  
}
theta=matrix(NA,nrow=n) # Vetor de parametros de quantidades desconhecidas e de n componentes
theta=start
for(i in 2:M) {
  
  x = runif(1)
  
  A = p(x,y)/p(theta[i-1],y)
  
  prob=min(1,A)
  
  u=runif(1)
  
  if(u < prob) {
    
    theta[i]=x
    
    taxa=taxa+1
    
  }
  
  else theta[i]=theta[i-1]
  
}
taxa=taxa/M
taxa
amostra <- as.mcmc(theta)
amostra1 <- mcmc(theta)
summary(amostra1)
HPDinterval(amostra,0.9)
plot(theta[2:n],col="red")
par(mfrow=c(3,1))
ts.plot(theta[1:100],main="Primeiras 100 execucoes",col="red")
points(1:100,theta[1:100],col="black",pch=19,cex=0.7)
ts.plot(theta[1:M],main="Todas as execucoes",col="red")
points(1:M,theta[1:M],col="black",pch=19,cex=0.7)
### Graficos
par(mfrow=c(3,1))
acf(theta,main="Todas as execucoes")
acf(theta[burn_in:M],main="Com Burn In")
acf(theta[seq(1,M,thin)],main="Com thin")
hist(theta,col = "red")
