library(dplyr)
library(ggplot2)
library(viridis)

#+ ANÁLISE EXPLORATÓRIA DO CONJUNTO FITBITDATA.
#+ Trabalho individual, a ser entregue como avaliação parcial no projeto mensal 03.
#+ 
#+ Baixar o conjunto de dados e realizar as atividades descritas abaixo.

#################################
# Carregar arquivo de dados
#################################
#+ O dataset utilizado foi obtido de 
#+ https://www.kaggle.com/datasets/panfordofori/fitbitdata

df <- read.csv("data/fitbit/activity_data_heartrate.csv")
df

boxplot(df$TotalDistance)
hist(df$TotalDistance)

shapiro.test(df$TotalDistance)

#######################################################################################
#+ 1. Apresentar a estatística descritiva (média, mediana, amplitude, desvio padrão e coeficiente de variação) dos atributos:
#+ TotalSteps, TotalDistance, TotalActiveMinutes, Heart_rate e Calories

summary(df$TotalSteps)
mean(df$TotalSteps)
median(df$TotalSteps)
sd(df$TotalSteps)
#amplitude
max(df$TotalSteps) - min(df$TotalSteps)
#Coeficiente de variação
(sd(df$TotalSteps) / mean(df$TotalSteps))*100

#+ CV <= 10% baixa dispersão
#+ 10% < CV <= 30% moderada
#+ CV >= 30% alta dispersão

#+ 2. Fazer o gráfico box-plot de todos os atributos numéricos. 
#+ Apresentar Q1, mediana e Q3 e fazer a análise sobre cada um deles.
boxplot(df$Heart_rate)



#+ Apresente o gráfico de dispersão dos atributos, TotalSteps e Calories; Faça a análise do gráfico;

plot(df$TotalSteps, df$Calories)



#+7. Assumindo que a frequência cardíaca média seja de 60 bpm. 
#+Crie um novo atributo categórico que separe em níveis: 
#+até 60 bpm, de 61 a 90 bpm e maior que 90 bpm. 
#+Calcule a média de passos por grupo e apresente na forma de gráfico de barras;


df <- df %>% mutate(faixa_freq = case_when(between(Heart_rate, 0, 60) ~ "Normal",
                                        between(Heart_rate, 61, 90) ~ "Alta",
                                        TRUE ~ "Muito Alta"))

aux <- df %>% group_by(faixa_freq) %>%
  summarise(mean_steps = mean(TotalSteps))


ggplot(aux, aes(x=faixa_freq, y=mean_steps, fill=faixa_freq))+
  geom_bar(stat="identity")


#+ 8. Construa uma tabela de frequências para a nova variável.
table(df$faixa_freq)



df %>% group_by(periodo) %>%
  summarize(total = n())


#+ 5. Fazer o diagrama de dispersão dos atributos, TotalSteps e TotalDistance.
#+ Apresentar a análise sobre ele.



#+ 6. Fazer o diagrama de dispersão dos atributos, TotalActiveMinutes e Heart_rate.
#+ Apresentar a análise sobre ele.
#+ 


#+ 7. Fazer o diagrama de dispersão dos atributos, TotalSteps e Calories
#+ Apresentar a análise sobre ele.
#+ 



#+ 8. Assumindo que a frequencia cardíaca média seja de 60 bpm. Crie um novo atributo categórico que separe 
#+ em níveis: até 60 bpm, de 61 a 90 bpm e maior que 90 bpm.
#+ 
#+ Criar um novo atributo categórico no dataset, que indique:
#+ três níveis de intensidade da atividade física, de acordo com a frequência cardiaca.
#+ Apresentar a estatística descritiva dos conjuntos e sua análise sobre eles.
#+ 




