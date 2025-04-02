setwd("H:/20250330 figure+code/Figure 1/Fig. 1h,i")
library(ggplot2)
library(RColorBrewer)


######################################## Evaluating peptides' hydrophobicity change after citrullination
rm(list =ls())
data = data.table::fread("Evaluating peptides' hydrophobicity change after citrullination.csv")

colormap <- rev(brewer.pal(20,"Spectral"))
ggplot(data, aes(`m_z value of unmodified peptides`,`m_z value of citrullinated peptides`))+
  stat_bin2d(bins=50)+geom_abline(intercept = 0, slope = 1, linewidth = .7)+
  scale_fill_gradientn(colors = colormap)+
  theme_classic()+
  scale_x_continuous(expand = c(0,0),limits = c(0,100))+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  theme(axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))

ggsave("Fig 1h.pdf", width = 3.8, height = 3)


######################################## Evaluating peptides' hydrophobicity change after deamidation
rm(list =ls())
data = data.table::fread("Evaluating peptides' hydrophobicity change after deamidation.csv")

colormap <- rev(brewer.pal(20,"Spectral"))
ggplot(data, aes(`m_z value of unmodified peptides`,`m_z value of deamidated peptides`))+
  stat_bin2d(bins=50)+geom_abline(intercept = 0, slope = 1, linewidth = .7)+
  scale_fill_gradientn(colors = colormap, breaks=c(100,300,500))+
  theme_classic()+
  scale_x_continuous(expand = c(0,0),limits = c(0,100))+
  scale_y_continuous(expand = c(0,0),limits = c(0,100))+
  theme(axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))

ggsave("Fig 1i.pdf", width = 3.8, height = 3)
