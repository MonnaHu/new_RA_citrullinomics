library(stringr)
library(tidyr)
library(dplyr)
library(data.table)


rm(list =ls())


d5 = read.csv("eFig 1i.csv")
names(d5)
d7 = reshape2::dcast(d5[,c( "protein" , "batch","IS")]%>%distinct(), protein ~ batch, value.var = "IS")

d7[,2:ncol(d7)] = lapply(d7[,2:ncol(d7)], log2)
cor=sapply(d7[,2:ncol(d7)],function(x){sapply(d7[,2:ncol(d7)],function(y){cor.test(x,y,method="spearman")[["estimate"]][["rho"]]})})
p=sapply(d7[,2:ncol(d7)],function(x){sapply(d7[,2:ncol(d7)],function(y){cor.test(x,y,method="spearman")[["p.value"]]})})
range(cor)

write.csv(cor,"IS_C_rho.csv",row.names = T)
write.csv(p,"IS_C_p_spearman.csv",row.names = T)

cor = read.csv("IS_C_rho.csv", row.names = 1)
cor1 = as.data.frame(cor)
for(i in 1:ncol(cor1)){cor1[i,i]=NA}
range(cor1, na.rm=T)
cor2 = reshape2::melt(cor1, measure.vars=1:66)
median(cor2$value, na.rm=T)

d<-read.csv("IS_C_rho.csv", row.names = 1)
library(pheatmap)
bk <- c(seq(0.4,1,by=0.01))

pdf("eFig 1i.pdf", width = 3.5, height = 3)
pheatmap(cor,scale = "none",cluster_col = F,cluster_rows=F,
         color = c(colorRampPalette(colors = c("#F5E3E3","#d17b78"))(length(bk))),
         legend_breaks=seq(0.4,1,0.2),
         breaks=bk,
         clustering_distance_rows = F,
         cluster_cols = F,
         show_colnames = F,
         show_rownames = F)
dev.off()
