#install.packages("DAAG")
library(DAAG)

glimpse(orings)

library(DT)
datatable(orings, class = 'cell-border stripe')

ggplot(orings, aes(x=Temperature, y=Total)) +
  geom_point(stat = "identity") +
  labs(x = 'Temperature (F)', y = 'Total O-Ring Damage') +
  scale_x_discrete(limits=25:80, breaks=seq(from=25, to=80, by=5)) +
  annotate("segment", x = 29, xend = 35, y = 0, yend = 2, colour = "pink", size=3, alpha=0.6, arrow=arrow())+
  geom_text(data=orings, aes(x=35, y=2.2, label="Temperatura prevista\n para lançamento: 26 e 29º"), color="red")+
  theme_bw()
