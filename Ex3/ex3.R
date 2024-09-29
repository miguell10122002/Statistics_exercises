 library(ggplot2)

# Leitura dos dados do arquivo
dados <- read.csv("GENDER_EMP_19032023152556091.txt", sep = "\t", header = TRUE)

filtered_data <- subset(dados, Country == "New Zealand" & IND == "EMP3" & Age.Group %in% c("15-24", "25-54", "55-64") & TIME == 2015)

# Criar o gráfico de barras
ggplot(filtered_data, aes(x = Age.Group, y = Value, fill = SEX)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Idades", y = "Taxa de desemprego (%)", fill = "Sexo") +
  scale_fill_manual(labels = c("Todos", "Homens", "Mulheres"), values = c("gray", "blue", "pink")) +
  ggtitle("Taxa de Desemprego na Nova Zelândia (2015)") +
  theme_minimal()

