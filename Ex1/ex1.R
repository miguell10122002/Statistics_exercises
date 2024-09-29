
library(readxl) 
library(lubridate) 
library(ggplot2)
excel_data <- read_excel("econ.xlsx", sheet = "IndicadoresEcon") 
filtered_data <- subset(excel_data, year(tempo) >= 1966)
mean_ndesemp <- mean(filtered_data$ndesemp) 
mean_tpp <- mean(filtered_data$tpp) 
sd_ndesemp <- sd(filtered_data$ndesemp) 
sd_tpp <- sd(filtered_data$tpp)
filtered_data$tpp <- ((filtered_data$tpp - mean_tpp)/sd_tpp) 
filtered_data$ndesemp <- ((filtered_data$ndesemp - mean_ndesemp)/sd_ndesemp)
df <- data.frame(x = filtered_data$tempo, y1 = filtered_data$tpp , y2 = filtered_data$ndesemp)
ggplot(data = df, aes(x = filtered_data$tempo)) +
  geom_line(aes(y = y1, color = "Taxa de Poupança Pessoal")) +
  geom_line(aes(y = y2, color = "Número de Desempregados")) +
  labs(title = "Comparação da taxa de poupança pessoal com o número de desempregados", x = "Anos") +
  scale_color_manual(values = c("green", "red"), labels = c("Taxa de Poupança Pessoal", "Número de Desempregados"))