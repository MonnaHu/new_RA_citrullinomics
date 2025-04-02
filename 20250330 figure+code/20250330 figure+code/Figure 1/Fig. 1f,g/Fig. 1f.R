setwd("H:/20250330 figure+code/Figure 1/Fig. 1f")

rm(list =ls())
all = data.table::fread("Mass error (ppm) of plasma and synovium samples.csv")

library(ggplot2)
ggplot(all, aes(`Mass error (ppm)`))+
  geom_histogram(fill="#5d51a3")+
  theme_classic()+
  scale_x_continuous(limits = c(-5,5))+
  scale_y_continuous(expand = c(0,0), limits = c(0,40000))+
  theme(axis.text = element_text(color="black"),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("Fig 1f.pdf",height = 3, width=3.5)



data = data.table::fread("Localization probability of plasma and synovium samples.csv")
df = as.data.frame(table(data$group))
df$Prob = df$Freq/sum(df$Freq) * 100
pie(df$Freq)
