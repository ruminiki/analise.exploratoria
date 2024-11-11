install.packages("PerformanceAnalytics")
library(dplyr)
library(stats)
library(gapminder)

#####################################
# Teste t - comparação de média da amostra
# com média da população
#####################################

# Um fabricante de baterias afirma que a vida útil média de suas baterias é de 1000 horas. 
# Um consumidor desconfia dessa afirmação e seleciona uma amostra de 30 baterias, 
# obtendo uma vida média de 980 horas com desvio padrão de 50 horas. 
# Realize um teste de hipóteses para verificar 
# se a vida útil média das baterias é realmente 1000 horas, 
# com um nível de significância de 0,05.

# Gera valores para a média e o desvio padrão definidos
data <- rnorm(30, mean=980, sd=50)
sd(data)
t.test(data, mu=1000)

#####################################
# Carregar arquivo de dados
# Fonte de dados: https://databank.worldbank.org/Life-Exp-Continents/id/436e78f7
#####################################

df <- read.csv("data/indicadores_sociais_continentes.csv")

as.data.frame(colnames(df))
#####################################
# EXERCÍCIO 01
#####################################

#+ Existe diferença significativa entre a média da expectativa de vida 
#+ entre países de baixa renda e o restante do mundo?

#+ TESTE BICAUDAL
#+ H0: Não há diferença entre as médias de expectativa de vida;
#+ H1: Há diferença entre as médias de expectativa de vida.

#análise evolução expectiva de vida
df %>%
  filter(continente %in% c("Low income", "World")) %>%
  ggplot(aes(x=ano, y=expectativa_vida_geral, color=continente)) +
  geom_line() +
  geom_point() + 
  ylim(0,100) +
  theme_minimal()

#t-test - comparação de médias
test_t <- df %>%
  filter(continente %in% c("Low income", "World")) %>%
  t.test(expectativa_vida_geral ~ continente, data=., conf.level = 0.95)

#imprime o resultado do teste
test_t

#formata o valor de p e verifica se é menor que o nível de significância
format(test_t$p.value, scientific=F)

# RESPOSTA
#+ Ao nível de confiança de 95% (p-valor < 0,001), é possível afirmar que existe diferença 
#+ significativa entre a média de expectativa de vida de países com baixa renda e 
#+ a média do restante do mundo.

#####################################
# EXERCÍCIO 02
#####################################
#+ Existe diferença significativa entre o acesso a água potável
#+ entre países da União Européia e América do Norte?

#+ TESTE BICAUDAL
#+ H0: Não há diferença entre as médias de acesso a água potável;
#+ H1: Há diferença entre as médias de acesso a água potável.

df %>%
  filter(continente %in% c("European Union", "North America")) %>%
  ggplot(aes(x=ano, y=acesso_agua_potavel, color=continente)) +
  geom_line() +
  geom_point() + 
  #ylim(0,100) +
  theme_minimal()

#t-test - comparação de médias
test_t <- df %>%
  filter(continente %in% c("European Union", "North America")) %>%
  t.test(acesso_agua_potavel ~ continente, data=., conf.level = 0.95)

#imprime o resultado do teste
test_t

#formata o valor de p e verifica se é menor que o nível de significância
format(test_t$p.value, scientific=F)

# RESPOSTA
#+ Ao nível de confiança de 95% (p-valor < 0,001), é possível afirmar que existe diferença 
#+ significativa entre a média de acesso a água potável entre países da União Européia e
#+ América do Norte.


#####################################
# EXERCÍCIO 03
#####################################
#+ Existe diferença significativa entre a expectativa de vida de mulheres 
#+ da América Latina e América do Norte?

#+ TESTE BICAUDAL
#+ H0: 
#+ H1: 


#####################################
# Carregar arquivo de dados
# Fonte de dados: https://databank.worldbank.org/Life-Exp-Continents/id/436e78f7
#####################################

df <- read.csv("data/world_data.csv")

#+ Existe diferença entre a média de acesso a eletricidade
#+ entre Paraguai e Brasil?

#+ TESTE BICAUDAL
#+ H0: A média de acesso a eletricidade é igual entre os países
#+ H1: A média de acesso é diferente

df %>%
  filter(pais %in% c("Paraguay", "Brazil")) %>%
  ggplot(aes(x=ano, y=acesso_eletricidade, color=pais)) +
  geom_line() +
  geom_point() + 
  ylim(0,100) +
  theme_minimal()

#t-test - comparação de médias
test_t <- df %>%
  filter(pais %in% c("Paraguay", "Brazil")) %>%
  t.test(acesso_eletricidade ~ pais, data=., conf.level = 0.95)

#imprime o resultado do teste
test_t

#formata o valor de p e verifica se é menor que o nível de significância
format(test_t$p.value, scientific=F)

#+ O acesso a eletricidade no Brasil é maior que no Paraguai?

#+ TESTE UNICAUDAL DIREITA
#+ H0: A média de acesso a eletricidade é igual entre os países
#+ H1: A média de acesso no Brasil é MAIOR que no Paraguai

df %>%
  filter(pais %in% c("Paraguay", "Brazil")) %>%
  ggplot(aes(x=ano, y=acesso_eletricidade, color=pais)) +
  geom_line() +
  geom_point() + 
  theme_minimal()

#t-test - comparação de médias
test_t <- df %>%
  filter(pais %in% c("Paraguay", "Brazil")) %>%
  t.test(acesso_eletricidade ~ pais, data=., conf.level = 0.95, alternative = "greater")

#imprime o resultado do teste
test_t

#formata o valor de p e verifica se é menor que o nível de significância
format(test_t$p.value, scientific=F)

