##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################

library(tidyverse)
library(dplyr)
library(caret)
library(ggplot2)
library(plotly)
library(jtools)
library(kableExtra)
library(equatiomatic)
library(correlation)
library(see)
library(ggraph)
library(PerformanceAnalytics)
library(nortest)
library(lavaan)
library(qgraph)
library(olsrr)
library(car)
library(mctest)

#data from https://databank.worldbank.org/Indicadores-sociais-1999-a-2023/id/7cca906c#

#carrega o dataset
dataset <- read.csv("data/social_ind_continents.csv", sep = ",")

# Informações básicas do banco de dados
dim(dataset)
glimpse(dataset)

#nomes das colunas anos
anos <- colnames(dataset[,6:12])

#renomeia colunas que não são anos
dataset <- dataset %>% 
  rename(indicador = 1,
         continente = 3)

#seleciona apenas registros válidos
dataset <- dataset[1:156,-c(2,4,5)]

#transforma as colunas ano uma coluna, repetindo o pais para cada ano
tmp <- pivot_longer(dataset,
                       cols = anos,
                       values_to = "valor")

tmp <- tmp %>% rename(ano = 3)

#transforma a serie em coluna
tmp <- pivot_wider(tmp,
                       id_cols = c("continente", "ano"),
                       names_from = c("indicador"),
                       values_from = "valor")

#padroniza o campo ano
tmp <- tmp %>% mutate(ano = substr(ano,2,5))

#renomear colunas
nome_colunas <- c("continente",
                  "ano",
                  "expectativa_vida_mulheres",
                  "expectativa_vida_homens",
                  "expectativa_vida_geral",
                  "prevalencia_diabetes",
                  "prevalencia_anemia",
                  "prevalencia_tabaco",
                  "prevalencia_hipertensao",
                  "prevalencia_sobrepeso_adultos",
                  "prevalencia_baixo_peso_criancas",
                  "crescimento_populacao_anual",
                  "acesso_agua_potavel",
                  "taxa_mortalidade_associada_higiene")
tmp<-setnames(tmp, 1:14, nome_colunas)

#converte para numérico
tmp <- tmp %>% mutate_at(c(3:14), as.numeric)

#salva o resultado em um novo arquivo
write.csv(tmp, "data/indicadores_sociais_continentes.csv", fileEncoding = "UTF-8",  row.names = FALSE)

