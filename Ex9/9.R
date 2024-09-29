set.seed(1918)

tamanho_amostra <- c(30, 50, 100, 200, 300, 500, 1000)
k <- 3000
p <- 0.6
nivel_conf <- 0.93
z <- qnorm((1 + nivel_conf)/2)

dif_media <- numeric(length(tamanho_amostra))

for (i in 1:length(tamanho_amostra)) {
  n <- tamanho_amostra[i]
  dif_comp <- numeric(k)
  
  for (j in 1:k) {
    amostra <- rbinom(n, 1, p)
    amostra_media <- mean(amostra)
    
    # Metodo 1
    proc <- function(p, amostra_media, n, z) {
      abs(amostra_media^2 - 2*amostra_media*p + p^2 - z^2*p*(1-p)/n)
    }
    p_valores <- seq(0, 1, 0.001)
    diferenca <- sapply(p_valores, proc, amostra_media=amostra_media, n=n, z=z)
    p_bai1 <- p_valores[which.min(diferenca[1:floor(length(diferenca)/2)])]
    p_cim1 <- p_valores[which.min(diferenca[(floor(length(diferenca)/2)+1):length(diferenca)]) + floor(length(diferenca)/2)]
    
    # Metodo 2
    se <- sqrt(amostra_media * (1-amostra_media) / n)
    p_bai2 <- amostra_media - z*se
    p_cim2 <- amostra_media + z*se
    

    dif_comp[j] <- (p_cim2 - p_bai2) - (p_cim1 - p_bai1)
  }
  
  dif_media[i] <- mean(dif_comp)
}

plot(tamanho_amostra, dif_media, type = "b", lwd = 2, 
     xlab = "Tamanho da amostra", ylab = "Diferenças médias",
     main = "Diferença entre os métodos")
