set.seed(1840)
n <- 1192
p <- 0.35
sample <- numeric(n)

# Simulação da distribuição geométrica de parâmetro p
for (i in 1:n) {
  u <- runif(1)
  x <- floor(log(1 - u) / log(1 - p))
  sample[i] <- x
}

# Cálculo da média e do desvio padrão amostrais
mean <- mean(sample)
std <- sd(sample)

# Cálculo da soma da média com o desvio padrão amostrais
sum_mean_std <- mean + std

# Contagem dos valores simulados superiores à soma da média com o desvio padrão amostrais
count <- sum(sample > sum_mean_std)

# Cálculo da proporção
proportion <- count / n

print(paste("A proporção é:", round(proportion, 4)))