#notas
notas <- c(8.02,8.02,7.91,8.08,7.42,8.62,8.60,8.76,
           8.38,7.91,8.38,8.10,7.92,8.28,7.23,8.66,
           8.40,7.38,7.60,7.98,7.14,8.84,7.98,8.86,
           7.3,7.93,7.44,8.03,7.93,7.66,7.88,7.36,
           7.78,8.33,8.84,8.60)

#teste de normalidade (se p < 0.05 não normal)
shapiro.test(notas)

hist(scale(notas))

mean(notas)
sd(notas)

norm_notas <- rnorm(100000, mean=mean(notas), sd=sd(notas))

hist(norm_notas, probability=TRUE)

#probabilidade de nota acima ou menor que 8.5 (lower.tail=T/F)
pnorm(8.5, mean=8.04, sd=0.49, lower.tail=T)

#probabilidade de nota acima de 8.5
qnorm(0.82, mean=8.04, sd=0.49)

#validando
sum(norm_notas > 8.5) / length(norm_notas)

dataset <- as.data.frame(rnorm(1000, mean=0, sd=5))
colnames(dataset) <- c("values") 

dataset <- rbind(dataset, data.frame(values = c(rnorm(200, mean=5, sd=3))))

ggplot(dataset, aes(values)) +
  geom_histogram(aes(y = stat(density)), binwidth = 0.2, color="black", fill="gray") +
  geom_vline(xintercept = mean(dataset$values), linetype = "dashed", size = 01,color="red")+
  geom_vline(xintercept = median(dataset$values), size = 02,color="orange")

mean(dataset$values)
median(dataset$values)

#################################
# DEMONSTRAÇÃO DA VARIÂNCIA
################################
d1 <- rnorm(1000, mean=3, sd=1.58)
d2 <- rnorm(1000, mean=3, sd=1.58)

#variancia
plot(d1, col="gray", bg="red", lwd=2)
abline(h=mean(d1), col=c("red"), lty=3, lwd=2)
abline(h=(mean(d1) - var(d1)), col=c("blue"), lty=3, lwd=2)
abline(h=(mean(d1) + var(d1)), col=c("blue"), lty=3, lwd=2)

#################################
# DEMONSTRAÇÃO COEFICIENTE DE VARIAÇÃO
################################
#CV >30%
d1 <- rnorm(1000, mean = 3, sd = 1.58)
(sd(d1)/mean(d1))*100
plot(d1)
hist(d1)
#CV 10 - 20%
d2 <- rnorm(1000, mean = 3, sd = 0.6)
(sd(d2)/mean(d2))*100
plot(d2, ylim=c(-2,8))
hist(d2, xlim=c(-2,8))
#CV 10%
d3 <- rnorm(1000, mean = 3, sd = 0.2)
(sd(d3)/mean(d3))*100
plot(d3, ylim=c(-2,8))
hist(d3, xlim=c(-2,8))

plot(d1)
sd(d1) / mean(d1) * 100

mean(d1)
sd(d1)

#################################
# PROBABILIDADE DISTRIBUIÇÃO NORMAL
################################
#utilizando o graphics (pacote nativo)
x = seq(0,20, by =0.01)
media = 10
var   = 4

# função de densidade
dx  = dnorm(x, mean = media, sd = sqrt(var))
#plot da função de densidade
plot(x, dx, type = "l", col = "blue", 
     ylab = bquote(f[X]~(x)~~"densidades"), 
     xlab = "x", 
     main =  bquote("N"~(mu==.(media)~","~sigma^2 == .(var))))


# poligono para representar a área sob a curva
a = 8 # Limite inferior
b = 12 # Limite superior
da = dnorm(a,mean = media, sd = sqrt(var))   # Densidade no Limite inferior crítico
db = dnorm(b,mean = media, sd = sqrt(var))   # Densidade no Limite superior crítico

polygon(x = c(a, a  , x[a<x & x<b], b), # X = conjunto dos valores de a até b
        y = c(0, da , dx[a<x & x<b], 0),          # Y = conjunto das Density de a até b
        col = "red",
        density = c(20),
        angle = c(-45))

# adicionado valores no eixo x
Map(axis, side=1, at = round(c(a,b),2),
    col.axis = c("red" , "red"),
    col.ticks = c("red", "red"),
    lwd=0, las=1,
    lwd.ticks = 2)

# cálculo da probabilidade de uma variável aleatória normal utilizando a função do R "pnorm"
# pnorm é a função cumulativa de probabilidade de uma distribuição normal
prob = pnorm(b, mean = media, sd = sqrt(var)) - pnorm(a, mean = media, sd = sqrt(var)) 

# legendas para o gráfico
legenda <- list( bquote( "Probabilidade =" ~ .(round(prob,4)) )  )
mtext(side = 3, do.call(expression, legenda), line=-2:-2, adj=1, col=c("red"))

####################################################

df <- read.csv("data/notas - notas.csv")
prova <- df$Prova

write.csv(df$Prova, "notas.csv", row.names = FALSE)

##################################
#teorema do limite central
#a medida que a amostra aumenta, tende a uma distribuição normal
amostra <- rnorm(1000, mean = mean(prova), sd = sd(prova))
hist(amostra)
shapiro.test(amostra)
##################################

hist(prova)
boxplot(prova)

#teste de normalidade (se p < 0.05 não normal)
#H0: Os dados seguem uma distribuição normal (p-valor > 0.05)
#H1: Os dados não seguem uma distribuição normal (p-valor < 0.05)
shapiro.test(prova)

prova <- prova[prova > 0]
hist(prova)
boxplot(prova)

shapiro.test(prova)

media <- mean(prova)
dp    <- sd(prova)

hist(prova)
abline(v=media, col=c("red"), lty=3, lwd=2)

#para facilitar a interpretação ajusta a média para 70
prova <- rnorm(500, mean = media, sd = dp)
hist(prova)
abline(v=media, col=c("red"), lty=3, lwd=2)

#probabilidade de nota maior que 81
pnorm(81, mean=media, sd=dp, lower.tail=F)

#probabilidade de nota entre 60 e 80
pnorm(60, mean=70, sd=10, lower.tail=F) - pnorm(60, mean=70, sd=10, lower.tail=T)

#probabilidade de nota acima ou menor que 8.5 (lower.tail=T/F)
pnorm(80, mean=70, sd=10, lower.tail=F)

################################
# Usando o valor de Z obtem-se 
# a probabilidade direta
################################
pnorm(-2)
