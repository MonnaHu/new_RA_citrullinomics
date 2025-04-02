library(ggplot2)

rm(list = ls())
dat = read.csv("eFig 1m.csv")
ggplot(dat, aes(variable, log2(value), color=group))+
  geom_boxplot(outlier.size = .4)+
  scale_color_manual(values = c("#3d7dae","#b22f36"))+
  theme_classic()+
  scale_x_discrete(expand = c(0.03,0.03))+
  scale_y_continuous(limits = c(-8,4), breaks = c(-8,-6,-4,-2,0,2,4), expand = c(0,0))+
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(),axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))
ggsave("eFig 1m.pdf", width = 5.5, height = 4)
