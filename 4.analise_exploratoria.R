install.packages(c("tibble", "dplyr", "ggplot2", "tidyr", "purrr"))
library(tibble)
library(dplyr)
library(ggplot2)
library(tidyr)
library(purrr)
library(viridis)
library(hrbrthemes)

#+ Neste exercício vamos tentar responder:
#+ Qual o percentual de aprovação na disciplina de Cálculo?
#+ Há diferenças nas médias finais e aprovações por sexo, raça e país de origem?
#+ Qual o país possui a melhor média na disciplina de cálculo?

# Para isso, crie uma coluna status:
# APR se nota >= 7 | REC se nota >= 5 e nota < 7 | REP se nota < 5

# Em seguida:
# Faça histograma da média final.
# Faça o box-plot da média final e responda:
# Quantos estudantes estão acima do terceiro quartil?

# CORRELAÇÃO
# Faça o gráfico de dispersão entre ira e média final, cr_total_integralizados e cr_total_pendentes;
# Faça o mapa de correlação e identifique:
# Existe correlação entre o número de faltas e a média final do estudante?
# Existe correlação entre a idade de ingresso no curso e a média final do estudante?

#####################################
# Carregar arquivo de dados
#####################################
df <- read.csv("data/world_data.csv")


#Correlação via regressão linear
plot(df$taxa_suicidio, df$consumo_alcool)
abline(fit <- lm(df$acesso_eletricidade ~ df$acesso_eletricidade_rural), col='red')
legend("topright", 
       legend=paste("R2 is", format(summary(fit)$adj.r.squared, digits=4)),
       col="red")

df %>% filter(pais == "Brazil") %>%
  ggplot(aes(y=crescimento_populacao, x=ano))+
  geom_line()


df %>% filter(pais %in% c("Brazil", "Paraguay", "Argentina")) %>%
  ggplot(aes(y=expectativa_vida_geral, x=ano, color=pais))+
  geom_line()

#cria variável para atribuir o status
df <- df %>% mutate(status = case_when(media_final < 5 ~ "REP",
                                       media_final >= 5 & media_final < 7 ~ "REC",
                                       media_final >= 7 ~ "APR"))

######################################################################
# Qual o percentual de aprovação na disciplina de Calculo?
######################################################################
#visualiza a distribuição por conceito
table(df$status)

#calcula o percentual de estudantes aprovados
aux <- df %>% group_by(status) %>% summarise(perc = n()/nrow(df)*100)
print(aux)

#+ GGPLOT2, é a principal biblioteca para a geração de gráficos do R.
#+ Ela funciona criando a estrutura de coordenadas, x e y, e adicionando camadas ao gráfico.
#+ Camadas podem ser barras, linhas, colunas, pontos.
#+ O primeiro argumento é o dataset, então ggplot(data = df), cria um gráfico vazio para o dataset 'df'.
#+ O segundo argumento é o aesthetic, e servem para mapear os dados para elementos visuais.
#+ 
#+ Links úteis:
#+ https://r-graph-gallery.com/
#+ https://r-charts.com/

#plota em gráfico de barras
ggplot(aux, aes(x=status, y=perc))+
  geom_bar(stat="identity")

ggplot(aux, aes(x=status, y=perc, fill=status))+
  geom_bar(stat='identity')+
  geom_text(aes(label=(paste(round(perc,digits = 2),"%"))), 
            color="black", position=position_dodge(width = 0.9), vjust=-0.30)+
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  xlab("") + ylab("%") + 
  theme(legend.position = c(0.5, -0.1), 
        legend.direction = "horizontal",
        legend.text = element_text(size = 10),
        legend.title = element_blank())

######################################################################
# Há diferenças nas médias finais e aprovações, por sexo, raça e país de origem?
######################################################################

df %>% ggplot(aes(x=media_final, fill=sexo)) +
  geom_density(alpha = 0.4) + 
  scale_fill_viridis(discrete = T) +
  facet_wrap(~ raca)

#os dados apontam para dois picos, próximo de zero, e entre 5 e 7
#pode-se verificar a média por status
mean(df$media_final[df$status == "APR"], na.rm = T)
mean(df$media_final[df$status == "REC"], na.rm = T)
mean(df$media_final[df$status == "REP"], na.rm = T)

#raça
df %>% ggplot(aes(x=status, fill=sexo)) +
  geom_bar(alpha = 0.4) + 
  scale_fill_viridis(discrete = T) +
  facet_wrap(~ raca)

#país
df %>% ggplot(aes(x=status)) +
  geom_bar(alpha = 0.4) + 
  scale_fill_viridis(discrete = T) +
  facet_wrap(~ pais_origem)

df %>% ggplot(aes(y=media_final)) +
  geom_boxplot(alpha = 0.4) + 
  scale_fill_viridis(discrete = T) +
  facet_wrap(~ pais_origem)

######################################################################
# Qual o país possui a melhor média na disciplina de cálculo?
######################################################################
#média geral
df_media_pais <- df %>% group_by(pais_origem) %>% 
  summarise(media = mean(media_final)) %>% 
  arrange(desc(media))

print(df_media_pais)

ggplot(df_media_pais, aes(x = reorder(pais_origem, media), y = media)) +
      geom_bar(stat="identity", alpha = 0.4, fill="blue") +
      geom_text(aes(label=round(media, digits=2)), 
               color="black", 
               position=position_dodge(width = 0.9), vjust=-0.30)+
      labs(title="",x="",y="") +
      ylim(0,10) +
      theme(axis.line = element_line(linewidth = 1, colour = "grey80"),
        axis.text.x = element_text(angle=90, hjust=1),
        panel.background = element_rect(fill="white"),
        panel.grid.minor.y = element_line(color="grey80"))

######################################################################
# Faça histograma da média final
######################################################################

#+ Histograma é uma representação gráfica da distribuição de frequências de uma variável quantitativa contínua. 
#+ Ele é um gráfico de barras, onde a altura de cada barra representa a frequência de ocorrência de um valor 
#+ ou intervalo de valores da variável.
df %>% ggplot(aes(x=media_final, fill=status)) +
  geom_histogram(alpha=0.6, binwidth = 1) +
  scale_fill_viridis(discrete = T)

df %>% ggplot(aes(x=media_final)) +
  geom_density(alpha=0.6)

df %>% ggplot(aes(x=media_final, fill=raca)) +
  geom_histogram(alpha=0.6, binwidth = 1) + 
  scale_fill_viridis(discrete=TRUE) +
  xlab("") +
  ylab("Quantidade matrículas") +
  facet_wrap(~raca)

######################################################################
# Faça o box-plot da média final e responda:
# Quantos estudantes estão acima do terceiro quartil?
######################################################################
#+ Conhecido como diagrama de caixa, é uma representação gráfica da distribuição de uma variável numérica. 
#+ Ele mostra a mediana, os quartis, os valores extremos e quaisquer outliers. 
#+ Um boxplot é dividido em três partes:
#+ A caixa central representa o interquartil, que é o intervalo entre o primeiro quartil (Q1) e o terceiro quartil (Q3). 
#+ A mediana (Q2) é representada por uma linha horizontal no meio da caixa.
#+ Os bigodes são as linhas que se estendem da caixa até os valores extremos. 
#+ Os valores extremos são definidos como os valores que estão fora de 1,5 vezes o interquartil.
#+ Os outliers são os pontos que estão fora dos bigodes.

ggplot(df, aes(y=media_final))+
  geom_boxplot()

# com a camada geom_jitter, possível mostrar os pontos de dados no boxplot
# útil para avaliar a quantidade de observações por grupo
ggplot(df, aes(x=sexo, y=media_final, fill=sexo))+
  geom_boxplot() + 
  geom_jitter(alpha = 0.2) +
  facet_wrap(~ raca)

ggplot(df, aes(y=media_final))+
  geom_boxplot(alpha=0.2) + 
  scale_fill_viridis(discrete=TRUE) +
  facet_wrap(~ pais_origem)

#encontrando o valor da nota limite no terceiro quartil (75%)
quantile(df$media_final, probs = seq(0, 1, 0.25))

# Quantos estudantes estão acima do terceiro quartil?
df %>% filter(media_final > 5.4) %>% summarise(total = n())

######################################################################
# CORRELAÇÃO
# Faça o gráfico de dispersão entre ira e média final, cr_total_integralizados e cr_total_pendentes.
# Faça o mapa de correlação e identifique:
# Existe correlação entre o numero de faltas e a média final do estudante?
# Existe correlação entre a idade de ingresso no curso e a média final do estudante?
######################################################################
ggplot(df, aes(x=media_final, y=ira))+
  geom_point(alpha=0.2) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(x=media_final, y=numero_faltas))+
  geom_point(alpha=0.2) +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(x=media_final, y=idade_ingresso_curso))+
  geom_point(alpha=0.2) +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(x=periodo_atual, y=cr_total_pendentes))+
  geom_point(alpha=0.2) +
  geom_smooth(method = lm, se = FALSE)

ggplot(df, aes(x=cr_total_pendentes, y=cr_total_integralizados))+
  geom_point(alpha=0.2) + 
  geom_smooth(method = "lm", formula = y ~ x, se = F, size = 1, level = 0.95) +
  theme_classic()

ggplot(df, aes(status, media_final)) + 
  geom_violin() +
  stat_summary(
    aes(
      y = stage(media_final, after_stat = 8),
      label = after_stat(paste(mean, "±", sd))
    ),
    geom = "text",
    fun.data = ~ round(data.frame(mean = mean(.x), sd = sd(.x)), 2)
  )

#################### CORRELAÇÃO ####################
#+ O Coeficiente de Correlação indica a força e a direção 
#+ da relação entre duas variáveis. 
#+ Em geral, a característica é boa se tiver uma forte correlação 
#+ com a variável resposta e não tenha forte correlação com outras características.
###################################################

# Separa um dataframe com as variáveis do tipo número
df.cor <- df[,colnames(select_if(df, is.numeric))]

# Cria a matriz de correlação das variáveis numéricas
correlacoes <- cor(df.cor)
view(correlacoes)

#plota o mapa das correlações
corrplot(correlacoes, method = 'circle', sig.level = 0.05)

#outro método de apresentar a correlação, nesse caso a função 
#chart.Correlation calcula a correlação e plota o gráfico
chart.Correlation(df.cor, histogram=FALSE, method = "pearson", pch=25)


#################### EXERCÍCIO PARA PRATICAR ####################
#+ No dataset existem alguns atributos que indicam o semestre atual (periodo_atual),
#+ os créditos cursados (cr_total_integralizados) e créditos pendentes (cr_total_pendentes).
#+ Considere que em um curso regular de 5 anos o aluno deverá cursar 10 semestres.
#+ Verifique, há alunos atrasados em relação ao prazo de conclusão do curso?
#+ Gere um gráfico de barras com os totais de alunos dentro do prazo e de alunos atrasados em relação ao prazo de conclusão.

#+ OBS: Exercício apenas para FINS DIDÁTICOS, uma vez que cada registro corresponde
#+ a uma matrícula em uma disciplina, portanto um estudante pode ter mais de uma matrícula.
#################################################################

df %>% group_by(status) %>% 
  ggplot(aes(x=status)) +
  geom_bar(alpha = 0.4, fill="blue")+
  geom_text(
    aes(
      y = after_stat(count + 2),
      label = after_stat(count)
    ),
    stat = "count"
  )

df %>%
  ggplot(aes(x = media_final)) +
  geom_histogram(aes(y = after_stat(density))) +
  geom_density()

ggplot(df, aes(x=status)) +
  geom_bar()




