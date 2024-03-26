## Gibbs Sampler para regressao
gibbs_lm <- function(alpha0, beta0, sigma0, parametros_alpha, parametros_beta,
                     parametros_sigma y, x, N){
  amostra_gibbs <- matrix(NA, nrow = N, ncol = 3)
  a1 <- parametros_alpha[1]; b1 <- parametros_alpha[2]
  a2 <- parametros_beta[1]; b2 <- parametros_beta[2]
  c0 <- parametros_sigma[1]; d <- parametros_sigma[2]
  alpha <- alpha0
  beta <- beta0
  sigma <- sigma0
  for (ii in 1:N) {
    #atualizacao dos parametros
    mu_alpha <- (mean(y)*n*sigma^(-2) +b1^(-1)*a1)/(n*sigma^(-2) +b1^(-1))
    sigma_alpha <- (n*sigma^(-2) +b1^(-1))^(-1)
    #amostrando da dist condicional
    6
    alpha <- rnorm(mu_alpha, sqrt(sigma_alpha), 1)
    #atualizacao dos parametros
    mu_beta <- (sigma^(-2)*t(y)%*%(x-mean(x)) +b2^(-1)*a2)
    mu_beta <- mu_beta/(sigma^(-2)*t((x-mean(x)))%*%(x-mean(x)) +b2^(-1))
    sigma_beta <- (sigma^(-2)*t((x-mean(x)))%*%(x-mean(x)) +b2^(-1))^(-1)
    #amostrando da dist condicional
    beta <- rnorm(mu_beta, sqrt(sigma_beta), 1)
    #atualizacao dos parametros
    c <- c0 + n/2
    d <- (sum(y - alpha -beta*(x -mean(x))) +2*d0)/2
    #amostrando da dist condicional
    sigma <- rinvgamma(1, c, d)
    amostra_gibbs[ii,] <- c(alpha, beta, sigma)
  }
  return(amostra_gibbs)
}