library(dplyr)
library(kableExtra)
library(ggplot2)
library(jtools)
library(equatiomatic)
################################################################################
#                             REGRESSÃO LINEAR SIMPLES                         #
################################################################################
df <- read.csv("data/study_performance.csv", sep=",")

################################################################################
#                            GRÁFICO DE DISPERSÃO                              #
################################################################################
df %>% 
  ggplot(aes(x = writing_score, y = reading_score)) +
  geom_point(color = "#39568CFF", size = 2.5) +
  geom_smooth(aes(color = "Fitted Values"), method = "lm", formula = y ~ x, se = F, size = 2) +
  labs(x = "Mortalidade Infantil", y = "Expectativa de Vida", title = paste("R²:",
                     round(((cor(df$writing_score, df$reading_score, use = "complete.obs"))^2),4))) +
  scale_color_manual("Legenda:", values = "grey50") +
  theme_classic()


################################################################################
#           MODELAGEM DE UMA REGRESSÃO LINEAR SIMPLES PARA O EXEMPLO 01        #
################################################################################
#Estimando o modelo
modelo <- lm(formula = writing_score ~ reading_score, data = df)

#Observando os parâmetros do modelo_tempodist
summary(modelo)

#Visualização do modelo no ambiente Viewer
#função 'extract_eq' do pacote 'equatiomatic'
extract_eq(modelo, use_coefs = T) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped",
                full_width = F,
                font_size = 28)

#Salvando fitted values (variável yhat) e residuals (variável erro) no dataset
df$yhat <- modelo$fitted.values
df$erro <- modelo$residuals

#Visualizando a base de dados com as variáveis yhat e erro
df %>%
  select(reading_score, writing_score, yhat, erro) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 22)

# Fórmula regressão linear
# y = a + bx
# a: intercepto
# b: coeficiente angular
nota_leitura <- 73
nota_escrita <- -0.67 + 0.99 * nota_leitura
nota_escrita

#Fazendo predições 
predict(object = modelo, data.frame(reading_score = nota_leitura))

