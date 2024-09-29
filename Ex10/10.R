set.seed(1613)

m <- 150
n <- 48
mu <- 63.4
sigma <- sqrt(4)
mu0 <- 62.2
alpha <- 0.1

# Gerar m amostras de tamanho n
amostras <- replicate(m, rnorm(n, mean = mu, sd = sigma))

# Aplicar o teste t para cada amostra e verificar se H0 é rejeitada
resultados <- apply(amostras, 2, function(x) t.test(x, mu = mu0)$p.value > alpha)

# Calcular a proporção de testes que levaram à não rejeição de H0
prop_nao_rejeicao <- mean(resultados)

# Arredondar o resultado para 3 casas decimais
prop_nao_rejeicao <- round(prop_nao_rejeicao, 3)

prop_nao_rejeicao
