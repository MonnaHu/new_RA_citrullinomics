rm(list=ls())
library(GGally)
library(reshape2)
library(dplyr)


rm(list =ls())

re = read.csv("eFig 1g.csv")
cor = reshape2::dcast(re, ID1~ID2, value.var = "cor")
library(pheatmap)
bk <- c(seq(0.4,1,by=0.01))
pheatmap(cor[,2:ncol(cor)],scale = "none",cluster_col = F,cluster_rows=F,
         color = c(colorRampPalette(colors = c("#F5E3E3","#B42620"))(length(bk))),
         legend_breaks=seq(0.4,1,0.2),
         breaks=bk,
         clustering_distance_rows = F,
         cluster_cols = F,
         show_colnames = F,
         show_rownames = F)
re1 = re[!(re$ID1==re$ID2),]
range(re1$cor, na.rm=T)
median(re1$cor, na.rm=T)

