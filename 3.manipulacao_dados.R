library(dplyr)
library(ggplot2)
library(purrr)

#################################
# Carregar arquivo de dados
#################################
#+ O dataset utilizado foi obtido de 
#+ https://www.kaggle.com/datasets/kalacheva/london-bike-share-usage-dataset

df <- read.csv("data/study_performance.csv")

#------------------- Conceitos básicos de manipulação de dados -----------------
# A manipulação dos dados consiste em organizar as variáveis e observações
# Na grande maioria dos casos, as bases de dados precisam ser preparadas

#Visualizar dataframe
#str | glimpse | head | nrow | colnames | dim | names | table
str(df) #estrutura do dataset
glimpse(df) #visão geral, tipos de dados e primeiros registros
head(df, n=10) #exibe os n primeiros registros
nrow(df) #número de linhas do dataset
ncol(df) #número de colunas do dataset
colnames(df) # =names(df)
dim(df) #dimensões do dataset
table(df$gender) 
table(df$race_ethnicity)
table(df$gender, df$race_ethnicity)
summary(df) #resumo estatístico do dataset
boxplot(df$math_score) #outlier
unique(df$parental_level_of_education) #lista os valores únicos de uma coluna

####### ACESSANDO VARIÁVEIS DO CONJUNTO DE DADOS (R-BASE) #######
df$gender #retorna um vetor
df['gender'] #retorna um dataframe
df[1] #pela posição da coluna, retorna dataframe

#primeira linha, terceira coluna
df[1,3]

#quinta linha, todas as colunas
df[5, ]

#a seleção pode ser feita por exclusão
df[5,-c(2:5)]

# Também é possível filtrar observações por meio dos operadores:
# Alguns operadores úteis para realizar filtros:

#+ "== igual"
#+ "> maior"
#+ ">= maior ou igual"
#+ "< menor"
#+ "<= menor ou igual"
#+ "!= diferente"
#+ "& indica e"
#+ "| indica ou"
df1 <- df[df$math_score >= 70,]

#percentual de alunos com nota maior que 7 em matemática
count(df1) / nrow(df) * 100

#alunos do grupo B, com média maior ou igual a 7
brupoB_media_maior_8 <- df[df$race_ethnicity == "group B" & df$math_score >= 80,]

#percentual de alunos do grupo B com media >= 8
count(brupoB_media_maior_8) / nrow(df[df$race_ethnicity == "group B",]) * 100

#raça de estudantes brasileiros
female_math_reading <- df[df$gender == 'female', c('gender', 'math_score', 'reading_score')]

#percentual de alunos brasileiros por raça
gender_lunch <- as.data.frame(table(df$gender, df$lunch))
gender_lunch
names(gender_lunch) <- c("gender", "lunch", "total")
gender_lunch

#percentual de tipo de almoço por gênero
gender_lunch$percentual <- gender_lunch$total / sum(gender_lunch$total) * 100
view(gender_lunch)

#plota em gráfico de barras
ggplot(gender_lunch, aes(x=gender, y=total, fill=lunch))+
  geom_bar(stat="identity")

gender_lunch %>% ggplot(aes(x=gender, y=total)) +
  geom_bar(stat="identity") +
  facet_grid(~ lunch)

#####################################
# DPLyR
#####################################
#+ O dplyr é um pacote de R que fornece uma interface para a manipulação de dados. 
#+ Ele é baseado na ideia de uma "gramática de manipulação de dados", que consiste 
#+ em uma série de verbos que podem ser usados para realizar tarefas comuns de manipulação de dados.
#+ 
#+ Nesta sessão vamos aprender a usar os cinco principais verbos:
#+ filter()
#+ arrange()
#+ select()
#+ mutate()
#+ summarise()

#reset dataset
df <- read.csv("data/study_performance.csv")
########## Filter #####################
#rbase
df[df$race_ethnicity == "group B",]

#dplyr
filter(df, race_ethnicity == "group B")

#usando pipe (%>%)
df %>% filter(race_ethnicity == "group B")
df %>% filter(race_ethnicity %in% c("group A", "group B"))
aux <- df %>% filter(between(math_score, 70,80))

hist(aux$math_score)

########## Select #####################

df2 <- df %>% select(gender, race_ethnicity, lunch)
df3 <- df %>% select(everything(), -gender)
df4 <- df %>% select(math_score:writing_score)

########## Mutate ##################
#cria nova coluna, com a média e conceito do estudante
df <- df %>% 
  mutate(media_notas = round(((math_score+reading_score+writing_score)/3),2)) %>%
  mutate(conceito = case_when(media_notas >= 80 ~ "A", 
                              media_notas >= 60 & media_notas < 80 ~ "B",
                              media_notas < 60 ~ "C"))

#ver distribuição do conceito
table(df$lunch, df$conceito)

#plota no gráfico de pontos
plot(df$media_notas)
abline(h=mean(df$media_notas), col=c("orange"), lty=3, lwd=5)

#printa uma linha separando os outliers
outliers <- boxplot.stats(df$media_notas)$out
abline(h=max(outliers), col=c("red"), lty=3, lwd=5)

########## Arrange ###################
# A função "arrange" faz a ordenação do dataset
# Se retirar o desc(), fica na ordem crescente
df5 <- df %>% arrange(desc(media_notas))
df5

########## Summarise##################
#Cria um resumo dos dados, é util em conjunto com a função de grupo group_by

#média e mediana por raça
df %>% group_by(race_ethnicity) %>% 
  summarise(media_notas = mean(media_notas, na.rm = T))

#calcula um conjunto de médias por raca
df %>% group_by(race_ethnicity) %>% 
  summarise_at(c("math_score", "reading_score", "writing_score", "media_notas"), mean, na.rm = TRUE)

######################################
# ANÁLISE DA MÉDIA
######################################

ggplot(df, aes(media_notas)) +
  geom_histogram(binwidth = 5) +
  theme_bw() +
  facet_wrap(~gender)

hist(df$media_notas, breaks = sqrt(length(df$media_notas)))
abline(v=mean(df$media_notas), col=c("orange"), lty=3, lwd=5)
abline(v=median(df$media_notas), col=c("blue"), lty=3, lwd=5)

ggplot(df, aes(media_notas, fill=gender)) +
  geom_density(adjust = 2, alpha = 0.5)

ggplot(df, aes(media_notas, fill=lunch)) +
  geom_density(adjust = 2, alpha = 0.5)

ggplot(df, aes(media_notas, fill=race_ethnicity)) +
  geom_density(adjust = 1.5, alpha = 0.4) +
  facet_wrap(~ race_ethnicity, ncol=1)

ggplot(df, aes(math_score, fill=race_ethnicity)) +
  geom_histogram() +
  facet_wrap(~race_ethnicity, ncol=1)

ggplot(df, aes(x=math_score, y=reading_score)) +
  geom_point() + 
  facet_wrap(~race_ethnicity, ncol=1)

# com a camada geom_jitter, possível mostrar os pontos de dados no boxplot
# útil para avaliar a quantidade de observações por grupo
ggplot(df, aes(x=gender, y=media_notas, fill=gender))+
  geom_boxplot() + 
  geom_jitter(alpha = 0.2)

#visualizar a distribuição de notas em relação a uma variável categórica
boxplot(df$math_score ~ df$gender)

#usando ggplot
ggplot(df, aes(x=gender, y=media_notas, fill=gender))+
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) +
  facet_wrap(~lunch) + 
  theme_bw()

#+ Até aqui, quais análises podem ser feitas?
#+ 


############# TRANSFORMAÇÃO DE DADOS ############
#+ Na etapa de transformação de dados, o 
#+ dplyr::mutate é um comando bastante utilizado,
#+ ele permite adicionar ou modificar uma coluna.
#################################################
#+ O dataset utilizado foi obtido de 
#+ https://www.kaggle.com/datasets/kalacheva/london-bike-share-usage-dataset

df <- read.csv("data/london_bike.csv")


#criar uma coluna para hora, separado da data
df <- df %>% mutate(Start.hour = strsplit(Start.date," ") %>% map_chr(., 2))
df <- df %>% mutate(Start.minute = strsplit(Start.hour,":") %>% map_chr(., 2))
df <- df %>% mutate(Start.hour = strsplit(Start.hour,":") %>% map_chr(., 1))

df <- df %>% mutate(End.hour = strsplit(End.date," ") %>% map_chr(., 2))
df <- df %>% mutate(End.minute = strsplit(End.hour,":") %>% map_chr(., 2))
df <- df %>% mutate(End.hour = strsplit(End.hour,":") %>% map_chr(., 1))

#formata a data
df <- df %>% mutate(Start.date =as.Date(Start.date,format = "%m/%d/%y"))
df <- df %>% mutate(End.date =as.Date(End.date,format = "%m/%d/%y"))

#converte os novos valores para dados numéricos
df$Start.hour <- as.numeric(df$Start.hour)
df$Start.minute <- as.numeric(df$Start.minute)
df$End.hour <- as.numeric(df$End.hour)
df$End.minute <- as.numeric(df$End.minute)

#indicar se a viagem ocorreu de madrugada, manhã, tarde ou noite
df <- df %>% mutate(periodo = case_when(between(Start.hour, 0, 5) ~ "Madrugada",
                                        between(Start.hour, 6, 11) ~ "Manha",
                                        between(Start.hour, 12, 17) ~ "Tarde",
                                        TRUE ~ "Noite"))

#ANÁLISE DAS VIAGENS, PERCENTUAL POR PERÍODO
#conta quantas viagens por período
aux <- df %>% group_by(periodo) %>%
  summarize(total = n())

#calcula o percentual por período
aux <- aux %>% mutate(percentual = total/sum(total) * 100)
aux

#plota em gráfico de barras
ggplot(aux, aes(x=periodo, y=percentual))+
  geom_bar(stat="identity") + 
  coord_flip()

############# Média de tempo, em minutos, por viagem #####
# converte milisegundos para minutos
df <- df %>% 
  mutate(Total.duration.min = (Total.duration..ms./60000)) 

#analisa a distribuição dos dados
boxplot(df$Total.duration.min)
hist(df$Total.duration.min)

#há grande concentração, no entanto existem pontos muito discrepantes
plot(df$Total.duration.min)

#pontos discrepantes estão afetando a média
summary(df$Total.duration.min)
mean(df$Total.duration.min)
median(df$Total.duration.min)

#+ Encontrando os outliers
#+ Consideram-se outliers, aqueles pontos que estejam fora do intervalo:
#+ Limite superior: Q3 + (1,5 * IQR) (intervalo interquartil)
#+ Limite inferior: Q1 1 (1,5 * IQR)
Q3 <- quantile(df$Total.duration.min, 0.75)
Q1 <- quantile(df$Total.duration.min, 0.25)

#calcula o intervalo interquartil
IQR <- Q3 - Q1

limite_superior <- Q3 + (1.5 * IQR)
limite_inferior <- Q1 - (1.5 * IQR)

summary(df$Total.duration.min)

#+ O tratamento de outliers pode seguir o mesmo tratamento dado aos NAs:
#+ remoção ou imputação de um valor. No nosso caso, vamos atualizá-lo para a
#+ mediana, pois não vai influenciar a variação dos demais pontos.
df$Total.duration.min[df$Total.duration.min > limite_superior] <- median(df$Total.duration.min)
df$Total.duration.min[df$Total.duration.min < limite_inferior] <- median(df$Total.duration.min)

plot(df$Total.duration.min)

boxplot(df$Total.duration.min)
hist(df$Total.duration.min)

summary(df$Total.duration.min)

#forma mais simples
min(boxplot.stats(df$Total.duration.min)$out)


########### Mês do Ano ######################
df <- df %>% 
  mutate(mes = format(as.Date(df$Start.date,), "%m"))

df <- df %>% 
  mutate(ano = format(as.Date(df$Start.date,), "%y"))

#usando group by
df %>% group_by(mes) %>% summarise(n())




#+ DESAFIO1: descubra quais horas do dia há maior movimento, e 
#+ sugira qual horário é o mais indicado para fazer a manutenção das bicletas e estações

#podem ser feitas de diferentes formas. 
# (1) contar e plotar em gráfico de barras
#install.packages("viridis")
library(viridis)
aux <-df %>% group_by(periodo, Start.hour) %>%
  summarise(total = n()) %>% 
  arrange(total)

aux %>% ggplot(aes(x=Start.hour, y=total, fill=periodo)) +
  geom_bar(stat='identity') + 
  scale_fill_viridis_d()+
  scale_x_discrete(limits=0:23, breaks=seq(from=0, to=23, by=1))

#ou (2) apenas rodar um histograma da coluna Start.hour
df %>% ggplot(aes(Start.hour, fill=periodo)) +
  geom_bar(stat = "count", binwidth = 1)+
  scale_fill_viridis_d()+
  scale_x_discrete(limits=0:23, breaks=seq(from=0, to=23, by=1))




#+ DESAFIO2: No geral, qual modelo de bicicleta é o mais utilizado?
aux <-df %>% group_by(Bike.model) %>%
  summarise(total = n()) %>% 
  arrange(total)

df %>% ggplot(aes(Bike.model, fill=Bike.model)) +
  geom_bar(stat="count") + 
  scale_fill_viridis_d()




#+ DESAFIO3: Qual estação destino é a mais movimentada em cada período do dia?
df %>% 
  group_by(End.station, periodo) %>% #agrupa po estação e período
  summarise(total = n()) %>% #conta quantas viagens por estação e período
  group_by(periodo) %>% #agrupa o resultado apenas por período
  arrange(desc(total)) %>% #ordena pelo total, do maior para o menor
  filter(row_number()==1) #seleciona primeira linha de cada grupo




#+ DESAFIO4: Analise o tempo de viagem em relação:
#+ Ao tipo da bicicleta
df %>% group_by(Bike.model) %>% 
summarise_at(vars(Total.duration.min), list(~ mean(.), ~ median(.), ~ sd(.)))

#+ A hora do dia
aux <- df %>% group_by(Start.hour) %>% 
  summarise_at(vars(Total.duration.min), list(~ mean(.), ~ median(.), ~ sd(.))) %>%
  arrange(desc(mean))

aux

#+ A estação de destino
aux <- df %>% group_by(End.station) %>% 
  summarise_at(vars(Total.duration.min), list(~ mean(.), ~ median(.), ~ sd(.))) %>%
  arrange(desc(mean))

aux

#Histograma por dia do mês
#converte para factor, R vai interpretar como se fosse uma variável categórica
df$Start.date <- as.factor(df$Start.date)

df %>% ggplot(aes(Start.date)) +
  geom_bar(stat="count", binwidth = 1) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

#análise dia da semana
df <- df %>% mutate(Day.of.week = weekdays(as.Date(Start.date)))

#plota gráfico. Parâmetro stat="count" indica que o
#geom_bar vai fazer a contagem e sumarizar os dados.
#Observe que não há uma ordem nos dias da semana
df %>% ggplot(aes(x=Day.of.week, fill=Day.of.week)) +
  geom_bar(stat = "count")+
  scale_fill_viridis_d()

#Para ordená-los por dia de maior movimento.
#Primeiro, conta os dias de maior movimento
aux <-df %>% group_by(Day.of.week) %>%
  summarise(total = n()) %>% 
  arrange(total)
 
#Plota o gráfico usando o reorder. Nesse caso, o parâmetro identity indica que
#ele vai usar o valor que está vindo em y
aux %>% ggplot(aes(x=reorder(Day.of.week,total), y=total, fill=Day.of.week)) +
  geom_bar(stat = "identity")+
  coord_flip() +
  scale_fill_viridis_d()
