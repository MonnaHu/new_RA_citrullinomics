library(ggplot2)

rm(list = ls())
dat = read.csv("eFig 1k.csv")
ggplot(dat, aes(variable, log2(value), color=group3))+
  geom_boxplot(outlier.size = .2,size = 0.1, fatten = 3)+
  scale_color_manual(values = c("#3d7dae","#cf9254","#b4373d"))+
  theme_classic()+
  scale_y_continuous(limits = c(-8,4), breaks = c(-8,-6,-4,-2,0,2,4), expand = c(0,0))+
  scale_x_discrete(expand = c(0.03,0.03))+
  theme(axis.line.x = element_blank(), axis.ticks.x = element_blank(),axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))
ggsave("eFig 1k.pdf", width = 5.5, height = 4)
