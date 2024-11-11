#install.packages(c("tibble", "dplyr", "ggplot2", "tidyr", "purrr"))
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(viridis)
library(hrbrthemes)

#####################################
# Análise exploratória, comparação entre países
#####################################

unique(df$pais)

#expectativa de vida
df %>% filter(pais %in% c("Brazil", "France", "Poland", "United States", "China")) %>%
  ggplot(aes(y=expectativa_vida_geral, x=ano, color=pais))+
  geom_line() + 
  geom_point() + 
  theme_bw()


#acesso eletricidade
df %>% filter(pais %in% c("Brazil", "France", "Poland", "United States", "China", "Paraguay", "Uruguai", "Argentina")) %>%
  ggplot(aes(y=acesso_eletricidade, x=ano, color=pais))+
  geom_line() + 
  geom_point() + 
  theme_bw()

#consumo alcool
df %>% filter(pais %in% c("Brazil", "France", "Poland", "United States", "China", "Paraguay", "Uruguai", "Argentina")) %>%
  ggplot(aes(y=consumo_tabaco, x=ano, color=pais))+
  geom_line() + 
  geom_point() + 
  theme_bw()


