# 1. Calcular a probabilidade de X = 5 ou 6
P_X_5 <- log10(1 + 1/4)
P_X_6 <- log10(1 + 1/7)

P_X_5_ou_6 <- P_X_5 + P_X_6

# 2. Obter a fração de potências de dois no intervalo [2^2, 2^28] com o primeiro algarismo 5 ou 6
potencias_de_dois <- 2^(6:22)
primeiro_algarismo <- as.integer(substr(potencias_de_dois, 1, 1))

potencias_de_dois_5_ou_6 <- length(which(primeiro_algarismo %in% c(4, 7)))
frac_potencias_de_dois_5_ou_6 <- potencias_de_dois_5_ou_6 / length(potencias_de_dois)

# 3. Calcular o desvio absoluto entre os valores calculados em 1. e 2.
desvio_absoluto <- abs(P_X_5_ou_6 - frac_potencias_de_dois_5_ou_6)

# 4. Indicar este desvio arredondado a 4 casas decimais.
desvio_absoluto <- round(desvio_absoluto, 4)

P_X_5_ou_6
frac_potencias_de_dois_5_ou_6
desvio_absoluto

