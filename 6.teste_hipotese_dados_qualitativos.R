install.packages("PerformanceAnalytics")
library(dplyr)
library(tidyr)
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

#box plot mortalidade infantil
df %>% 
  filter(pais %in% c("Brazil", "France")) %>%
  select(pais, taxa_mortalidade_infantil) %>%
  ggplot(aes(y=taxa_mortalidade_infantil, x=pais)) +
  geom_boxplot()

aux <- df %>% 
  filter(ano == max(ano)) %>%
  mutate(transparency = cut(indice_transparencia_estado,
                                breaks = 3,
                                labels=c("Baixo", "Medio", "Alto"))) %>%
  mutate(mortalidade_infantil = cut(mortalidade_infantil,
                                breaks = 3,
                                labels=c("Baixa", "Media", "Alta"))) %>%
  select(transparency, pais, mortalidade_infantil)

chisq.test(aux$transparency, aux$mortalidade_infantil)  
table(aux$mortalidade_infantil, aux$transparency) %>% chisq.test()
