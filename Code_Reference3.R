# vetor de parâmtros
theta <- rep(0.25, 4)
# tamanho da amostra
n0 <- 100
amostra <- rmultinom(n0, 20, theta)
# parametros da priori
hiper_parametros <- rep(5, 4)
# parametros da posteriori
parametros_pos <- hiper_parametros + c(sum(amostra[1,]), sum(amostra[2,]),
                                       sum(amostra[3,]), sum(amostra[4,]))
# amostra a posteriori
library(MCMCpack)
amostra_pos <- rdirichlet(1000, parametros_pos)
# medidas resumo
summary(amostra_pos)
round(sd(amostra[,1]), 4)
round(sd(amostra[,2]), 4)
round(sd(amostra[,3]), 4)
round(sd(amostra[,4]), 4)
# graficos
dados_posteriori <- as.data.frame(amostra_pos)
colnames(dados_posteriori) <- c("theta1", "theta2", "theta3", "theta4")
head(dados_posteriori)
par(mfrow = c(2, 2))
hist(dados_posteriori$theta1, main = "Distribuição marginal de theta_1")
lines(density(dados_posteriori$theta1), col = "red")
hist(dados_posteriori$theta1, main = "Distribuição marginal de theta_2")
lines(density(dados_posteriori$theta1), col = "red")
hist(dados_posteriori$theta1, main = "Distribuição marginal de theta_3")
lines(density(dados_posteriori$theta1), col = "red")
hist(dados_posteriori$theta1, main = "Distribuição marginal de theta_4")
lines(density(dados_posteriori$theta1), col = "red")
# valores dos parâmetros
alpha <- 2
beta <- 3
sigma2 <- 5
x <- rnorm(100, 5, 0.2)
erro <- rnorm(100, 0, sqrt(sigma2))
y <- alpha + beta*x + erro
# parametros priori
a1 <- 2.1; b1 <- 100
a2 <- 2.9; b2 <- 100
c <- 10; d <- 2
n <- length(y)
# parametros poteriori -sigma2
c1 <- c + n/2
d1 <- d + sum(erro^2)/2
a1_pos <- ((sum(y -beta*x)/sigma2) -a1/b1)/(n/sigma2 + 1/b1)
b1_pos <- 1/(n/sigma2 + 1/b1)
a2_pos <- (((sum(y*x) - alpha*sum(x))/sigma2) -a2/b2)/(sum(x^2)/sigma2 + 1/b2)
b2_pos <- 1/(sum(x^2)/sigma2 + 1/b2)
sigma2_pos <- rinvgamma(1000, c1, d1)
alpha_pos <- rnorm(1000, a1_pos, sqrt(b1_pos))
beta_pos <- rnorm(1000, a2_pos, sqrt(b2_pos))
# Medidas resumo
summary(sigma2_pos)
round(sd(sigma2_pos), 3)
summary(alpha_pos)
round(sd(alpha_pos), 3)
summary(beta_pos)
round(sd(beta_pos), 3)
# Figuras
par(mfrow = c(2,2))
plot(x, y, main = "Gráfico de dispersão")
hist(sigma2_pos, main = "Distribuição marginal a posteriori de sigma2")
lines(density(sigma2_pos), col = "red")
hist(alpha_pos, main = "Distribuição marginal a posteriori de alpha")
lines(density(alpha_pos), col = "red")
hist(beta_pos, main = "Distribuição marginal a posteriori de beta")
lines(density(beta_pos), col = "red")
