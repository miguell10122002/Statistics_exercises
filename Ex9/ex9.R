set.seed(1918)


n_values <- c(30, 50, 100, 200, 300, 500, 1000)


k <- 3000  
p <- 0.6   
c <- 0.93 
z <- qnorm(((1+c)/2), mean = 0, sd = 1)
i <- 1
media_diferencas <- numeric(length(n_values))


gerador_amostras_bernoulli <- function(n) {
  matrix(rbinom(n * k, size = 1, prob = p), nrow = k, ncol = n)
}


for (n in n_values) {
  
  
  amostras <- gerador_amostras_bernoulli(n)
  diferenca_intervalos <- numeric(k)
  
  
  for (j in 1:k)  {
    
    x_media <- mean(amostras[j,])
    
    
    a <- 1+z^2/n
    b <- -2*x_media-z^2/n
    c <- x_media^2
    
    eq_matrix <- matrix(c(a, b, c), nrow = 1)
    
    intervalo_confianca_1 <- sort(Re(polyroot(eq_matrix)))
    
    
    desvio_padrao = sqrt(x_media * (1 - x_media) / n)
    margem_erro <- qnorm(1 - (1 - c) / 2) * desvio_padrao
    
    intervalo_confianca_2 <- c(x_media - margem_erro, x_media + margem_erro)
    
    
    diferenca_intervalos[j] <- ((intervalo_confianca_2[2] - intervalo_confianca_2[1]) 
                                   - (intervalo_confianca_1[2] - intervalo_confianca_1[1]))
  }
  media_diferencas[i] = mean(diferenca_intervalos)
  i <- i+1
}

plot(n_values, media_diferencas, type = "b", lwd = 3, 
     xlab = "Tamanho amostra", ylab = "Diferença médias",
     main = "Diferença entre Métodos")
