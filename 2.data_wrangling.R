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
dataset <- read.csv("data/raw_data.csv", sep = ",")

# Informações básicas do banco de dados
dim(dataset)
glimpse(dataset)

#nomes das colunas anos
anos <- colnames(dataset[,7:29])

#renomeia colunas que não são anos
dataset <- dataset %>% 
  rename(pais = 1,
         sigla_pais = 2,
         serie = 3,
         codigo_serie = 4)

#seleciona apenas registros válidos
dataset <- dataset[1:15407,-c(5,6)]

#transforma as colunas ano uma coluna, repetindo o pais para cada ano
tmp <- pivot_longer(dataset,
                       cols = anos,
                       values_to = "valor")

tmp <- tmp %>% rename(ano = 5)

#transforma a serie em coluna
tmp <- pivot_wider(tmp,
                       id_cols = c("pais", "sigla_pais", "ano"),
                       names_from = c("serie","codigo_serie"),
                       values_from = "valor")

#padroniza o campo ano
tmp <- tmp %>% mutate(ano = substr(ano,2,5))

as.data.frame(colnames(tmp))

#renomear colunas
cols <- read.csv("data/metadados.csv", sep = ";")
library(data.table)
# Rename columns from list
tmp<-setnames(tmp, 4:74, cols$new)

#converte para numérico
tmp <- tmp %>% mutate_at(c(4:74), as.numeric)

#salva o resultado em um novo arquivo
write.csv(tmp, "data/final_data.csv", fileEncoding = "UTF-8",  row.names = FALSE)
