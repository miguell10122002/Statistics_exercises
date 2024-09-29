# Set the seed
set.seed(1710)

# Generate a sample of size 162 from a Cauchy distribution
tamanho_amostra <- 176
localizacao <- 1.8
escala <- 1.4
probabilidade = seq(1, tamanho_amostra)/(tamanho_amostra + 1)

amostra_cauchy <- rcauchy(tamanho_amostra, localizacao, escala)
amostra_cauchy_ordenada <- sort(amostra_cauchy)

quantis_cauchy <- quantile(amostra_cauchy, probabilidade)

# Generate a sample from a normal distribution for comparison
normal_m <- 1.3
normal_var <- 3.8

amostra_normal <- rnorm(tamanho_amostra, normal_m, sqrt(normal_var))
amostra_normal_ordenada <- sort(amostra_normal)
quantis_normal = quantile(amostra_normal, probabilidade)

# Plot the results
plot(quantis_cauchy, amostra_cauchy_ordenada, type = "p", col = "purple",
     main = "Quantis de probabilidade - Distribuição Normal vs Cauchy",
     xlab = "Quantis",
     ylab = "Valores Ordenados")

points(quantis_normal, amostra_normal_ordenada, type = "p", col = "red")

abline(0, 1, lty = 2, col = "green")

legend("bottomright", legend = c("Cauchy", "Normal"),
       col = c("purple", "red"), pch = 1)