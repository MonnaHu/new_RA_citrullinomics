setwd("H:/20250330 figure+code/Figure 3/2. k-means clustering & Fig. 3i")
library(dplyr)
library(ComplexHeatmap)
library(circlize)
library(TCseq)
library(ggplot2)

rm(list = ls())
result=read.csv("wilcox result atleast5 checked.csv", header = T,row.names = 1)
names(result)
dat=result[,c("median_HC","median_PRA","median_RAA")]

df<-as.matrix(dat)
set.seed(123)
cluster_num<-4
cluster<-timeclust(df, algo='km',k=cluster_num, standardize = TRUE)

citsite_cluster <-data.frame(cluster@cluster) ;citsite_cluster$ID=rownames(citsite_cluster)
table(cluster@cluster)
dat$ID=rownames(dat)
citsite_cluster1 <- merge(citsite_cluster,dat,by="ID")
write.csv(citsite_cluster1,"cluster result.csv",row.names = F)

col_fun = colorRamp2(c(-1,0, 1), c("#5D669F","white","#AF322F"))
top_annotation = HeatmapAnnotation(df=data.frame(group=rep(c("Health","preRA","RA"), c(1,1,1))),
                                   col = list(group=c(RA="#BF5960",Health="#6F99AD",preRA="#F9A363"))) 
pdf("Heatmap cluster.pdf",width = 3, height = 6)
Heatmap(t(scale(t(citsite_cluster1[,3:5]))),name = " ",
        col = col_fun,
        top_annotation = top_annotation,
        row_split = citsite_cluster1$cluster.cluster,
        cluster_columns = F,
        cluster_rows = F,
        show_heatmap_legend = T,
        border = F,
        show_column_names = F,
        show_row_names = F,
        column_title = NULL)
dev.off()

citsite_cluster2=cbind(citsite_cluster1[,1:2],t(apply(citsite_cluster1[,3:5],1,scale)))
cc=reshape2::melt(citsite_cluster2,id.vars=1:2, measure.vars=3:5)

ggplot(cc)+
  geom_line(aes(variable, scale(value), color=as.factor(cluster.cluster), group=ID))+
  geom_boxplot(aes(variable, scale(value)),color="black", outlier.colour = NA, width=.4, linewidth=.4)+
  facet_wrap(.~cluster.cluster, nrow=4, scale="free")+
  geom_hline(yintercept = 0,color="#BBBBBB", linetype=2, linewidth=.4)+
  scale_y_continuous(breaks=c(-1,0,1))+
  scale_x_discrete(expand = c(0.12,0.12))+
  theme_classic()+
  labs(x="")+
  scale_color_manual(values = c("#F9A363","#AF322F","#8A86BC","#67A596"))+
  theme(legend.position = "none",
        axis.line = element_line(linewidth = .8),
        axis.ticks = element_line(linewidth=.8),
        axis.ticks.length = unit(1.25,"mm"),
        strip.background = element_blank())
ggsave("clusters.pdf", width = 3, height = 11)

