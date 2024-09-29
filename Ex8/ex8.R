set.seed(1105)
tamanho_amostra <- 114
localizacao <- 0.4
escala <- 1.8
probabilidade = seq(1, tamanho_amostra)/(tamanho_amostra + 1)
amostra_cauchy <- rcauchy(tamanho_amostra, localizacao, escala)
amostra_cauchy_ordenada <- sort(amostra_cauchy)
quantis_cauchy <- quantile(amostra_cauchy, probabilidade)
normal_m <- 1.9
normal_var <- 4
amostra_normal <- rnorm(tamanho_amostra, normal_m, sqrt(normal_var))
amostra_normal_ordenada <- sort(amostra_normal)
quantis_normal = quantile(amostra_normal, probabilidade)
plot(quantis_cauchy, amostra_cauchy_ordenada, type = "p", col = "yellow",
     main = "Quantis de probabilidade - Distribuição Normal vs Cauchy",
     xlab = "Quantis",
     ylab = "Valores Ordenados")
points(quantis_normal, amostra_normal_ordenada, type = "p", col = "red")
abline(0, 1, lty = 2, col = "purple")
legend("bottomright", legend = c("Cauchy", "Normal"),
       col = c("green", "red"), pch = 1)