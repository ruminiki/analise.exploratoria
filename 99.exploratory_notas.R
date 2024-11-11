
df <- read.csv("data/notas - notas.csv")

df <- data.frame(lapply(df, function(x) gsub(",", ".", x)))
df$Revisão <- as.numeric(df$Revisão)
df <- df %>% mutate(Revisão = Revisão * 100)
df$Prova <- as.numeric(df$Prova)
df$UAs <- as.numeric(df$UAs)
df <- df %>% mutate(UAs = UAs * 100)

plot(df$UAs, df$Prova)
plot(df$Revisão, df$Prova)

sum(count(df$UAs[df$UAs>75])$freq)
14/37
# Vertical box plot
boxplot(df$Prova, col = "white")

# Points
stripchart(df$Prova,              # Data
           method = "jitter", # Random noise
           pch = 19,          # Pch symbols
           col = 4,           # Color of the symbol
           vertical = TRUE,   # Vertical mode
           add = TRUE)        # Add it over

df %>% 
  ggplot(aes(UAs)) +
  geom_histogram(binwidth = 10, colour = 4, fill = "darkseagreen3")+
  geom_vline(aes(xintercept=median(df$UAs)),col = 'red', size = 0.5)+
  scale_x_continuous(breaks = seq(0, 100, 10), minor_breaks = 20) +
  theme_bw()


boxplot(df$UAs, horizontal = F, axes = T,
        col = rgb(0, 0.8, 1, alpha = 0.5))



