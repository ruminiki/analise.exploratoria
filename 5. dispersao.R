install.packages("PerformanceAnalytics")
library(dplyr)
library(stats)
library(corrplot)
library(Hmisc)
library(RColorBrewer)
library(PerformanceAnalytics)

#####################################
# Carregar arquivo de dados
# Fonte de dados: https://databank.worldbank.org/Indicadores-sociais-1999-a-2023/id/7cca906c#
#####################################

df <- read.csv("data/world_data.csv")

#####################################
# Análise de correlação de atributos
#####################################

# Teste de correlação de Pearson
cor <- cor.test(df$taxa_populacao_acesso_saneamento, df$taxa_mortalidade_infantil, method=c("pearson"))
print(paste("A correlação é: ", cor$estimate))

plot(df$taxa_populacao_acesso_saneamento, df$taxa_mortalidade_infantil)

# Selecionar características de interesse
aux <- df %>% select(acesso_eletricidade, 
                     taxa_alfabetizacao_adultos, 
                     taxa_mortalidade_infantil,
                     consumo_energia,
                     consumo_tabaco,
                     consumo_alcool,
                     taxa_alfabetizacao_adultos,
                     gastos_governo_educacao,
                     subnutricao,
                     crescimento_populacao)

# Gera a matriz de correlação
cormat <- cor(aux, use="complete.obs")
corrplot(cormat, method = "color")

#Opção 2: Usando chart.Correlation, basta passar o data frame
chart.Correlation(aux, histogram=FALSE, method = "pearson")

# Opção 3
cormat <- cor(aux, use="complete.obs")
corrplot(cormat, method = "number")
corrplot(cormat, method = "circle")
corrplot(cormat, method = "square")
corrplot(cormat, method = "color")
corrplot(cormat, method = "pie")


#####################################
# ATIVIDADE
#+ Sua missão será encontrar atributos que tenham forte correlação, 
#+ pode ser positiva ou negativa (acima de 0.7 ou menor que -0.7). 
#+ Analise pelo menos três pares de variáveis. Explique a relação entre elas.
#####################################

#Exemplo tratamento de NAs
df$taxa_suicidio <- ifelse(is.na(df$taxa_suicidio), 
                           median(df$taxa_suicidio, na.rm = T), 
                           df$taxa_suicidio)








