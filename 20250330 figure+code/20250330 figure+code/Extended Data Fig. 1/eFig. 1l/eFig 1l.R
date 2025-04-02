library(ggplot2)

rm(list = ls())
dat = read.csv("eFig 1l.csv")
ggplot(dat, aes(variable, log2(value), color=group))+
  geom_boxplot()+
  scale_color_manual(values = c("#3583c9","#8b7ec0"))+
  theme_classic()+
  scale_y_continuous(limits = c(-12,6), breaks = c(-12,-9,-6,-3,0,3,6), expand = c(0,0))+
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(),axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))
ggsave("eFig 1l.pdf", width = 4.2, height = 3)
