set.seed(1718)  # Define a semente como 1718
m <- 2744  # Número de amostras
n <- 13  # Dimensão das amostras

# Gerar amostras de uma população normal de média zero e variância unitária
samples <- matrix(rnorm(m * n), nrow = m, ncol = n)

# Calcular a soma dos quadrados dos valores observados para cada amostra
sum_of_squares <- apply(samples, 1, function(x) sum(x^2))

# Calcular o quantil de probabilidade 0.39 da amostra das somas dos quadrados dos valores observados
quantile_sample <- quantile(sum_of_squares, probs = 0.39, type = 2)

# Calcular o quantil correspondente à distribuição teórica da soma de quadrados de variáveis normais reduzidas independentes
quantile_theoretical <- qchisq(0.39, df = n)

# Calcular a diferença em valor absoluto entre os dois quantis
diferenca <- abs(quantile_sample - quantile_theoretical)
diferenca <- round(diferenca, 4)  # Arredondar para 4 casas decimais

# Imprimir a diferença em valor absoluto
print(diferenca)
