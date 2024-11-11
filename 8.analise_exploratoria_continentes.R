install.packages("PerformanceAnalytics")
library(dplyr)
library(stats)
library(gapminder)
library(ggplot2)

#####################################################
# Carregar arquivo de dados
# Fonte de dados: https://databank.worldbank.org/Life-Exp-Continents/id/436e78f7
#####################################

df <- read.csv("data/indicadores_sociais_continentes.csv")


df %>% 
  filter(continente %in% c("North America", "Latin America & Caribbean")) %>%
  ggplot(aes(x=continente, y=expectativa_vida_geral), fill=continente) +
  geom_boxplot() + 
  theme_bw()




horas_estudo = c(10,12,15,17,18,20,25,25,26,27,30,25,32,35,36)
notas_finais = c(4,5,6,5,4,6,6.5,6.8,7,7.2,7.5,7.8,8,8.2,8.3)
plot(horas_estudo, notas_finais)




