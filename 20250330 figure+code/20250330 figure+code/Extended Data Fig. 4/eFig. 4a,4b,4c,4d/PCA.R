
library(dplyr)
library(factoextra)
library(FactoMineR)

rm(list=ls())

d = dir(pattern = ".csv");d
d1 = lapply(d, function(x){
  pca_sample = read.csv(x, row.names = 1)
  p <- ggplot(data = pca_sample, aes(x = Dim.1, y = Dim.2)) +
    geom_point(aes(color = `Responder.HM`), size = 2, alpha=1) + 
    scale_color_manual(values = c("#D72E2A","#3D7DAE")) + 
    theme(panel.grid = element_blank(), panel.background = element_rect(color = 'black', fill = 'transparent'), 
          legend.key = element_rect(fill = 'transparent'),
          strip.background = element_blank(),
          legend.position = "bottom",
          axis.text = element_text(color="black"),
          axis.line = element_line(linewidth=.8, color="black"),
          axis.ticks = element_line(linewidth=.8, color="black"),
          axis.ticks.length = unit(1.25,"mm")) + 
    labs(x =  paste('PCA1:', unique(pca_sample$pca_eig1), '%'), y = paste('PCA2:', unique(pca_sample$pca_eig2), '%'), color = '')+  
    stat_ellipse(aes(fill = `Responder.HM`), geom = 'polygon', level = 0.95, alpha = 0.2, show.legend = FALSE) +
    scale_fill_manual(values = c("#D72E2A","#3D7DAE"))
})

library(cowplot)
plot_grid(plotlist = d1, align = "h", nrow = 1)
ggsave("ML&MH  pca plasma1&2.pdf", height = 3, width = 9.8)


