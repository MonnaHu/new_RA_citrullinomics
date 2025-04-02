library(dplyr)
library(ComplexHeatmap)
library(circlize)


rm(list = ls())
data=readxl::read_xlsx("eFig 2f,2h,2j.xlsx", sheet=1)
names(data)
data[,4:14] = lapply(data[,4:14], as.numeric)
table(data$sig_PRA_HC)
table(data$sig_RAA_HC)
dat=data%>%filter(sig_PRA_HC=="up in PRA, wilcox" ,sig_RAA_HC!="up in RAA, <5 >40%" , sig_RAA_HC!="up in RAA, wilcox")%>%
  arrange(p_PRA_vs_HC)%>%data.frame()
rownames(dat)=dat$gene_position
dat1=dat[1:10,c("median_HC", "median_PRA", "median_RAA")]

col_fun = colorRamp2(c(-1,0, 1), c("#5D669F","white","#AF322F"))

top_annotation = HeatmapAnnotation(df=data.frame(group=c("Health","preRA","RA")),
                                   col = list(group=c(RA="#BF5960",Health="#6F99AD",preRA="#F9A363"))) 

pdf("Heatmap PRA_up_only_wilcox_top10.pdf", width = 5, height = 4)
Heatmap(t(scale(t(dat1))),col=col_fun,
        rect_gp = gpar(col="white", lwd=3),
        top_annotation = top_annotation,
        cluster_rows = F, cluster_columns = F,
        show_heatmap_legend = T,
        border = F, show_column_names = F,
        show_row_names = T, column_title = NULL)
dev.off()


rm(list = ls())
data=readxl::read_xlsx("eFig 2f,2h,2j.xlsx", sheet=1)
names(data)
data[,4:14] = lapply(data[,4:14], as.numeric)
table(data$sig_PRA_HC)
table(data$sig_RAA_HC)
dat=data%>%filter(sig_RAA_HC=="up in RAA, wilcox" ,sig_PRA_HC!="up in PRA, <5 >40%" , sig_PRA_HC!="up in PRA, wilcox")%>%
  arrange(p_RAA_vs_HC)%>%data.frame()
rownames(dat)=dat$gene_position
dat1=dat[1:10,c("median_HC", "median_PRA", "median_RAA")]

col_fun = colorRamp2(c(-1,0, 1), c("#5D669F","white","#AF322F"))

top_annotation = HeatmapAnnotation(df=data.frame(group=c("Health","preRA","RA")),
                                   col = list(group=c(RA="#BF5960",Health="#6F99AD",preRA="#F9A363"))) 

pdf("Heatmap RAA_up_only_wilcox_top10.pdf", width = 5, height = 4)
Heatmap(t(scale(t(dat1))),col=col_fun,
        rect_gp = gpar(col="white", lwd=3),
        top_annotation = top_annotation,
        cluster_rows = F, cluster_columns = F,
        show_heatmap_legend = T,
        border = F, show_column_names = F,
        show_row_names = T, column_title = NULL)
dev.off()



rm(list = ls())
data=readxl::read_xlsx("eFig 2f,2h,2j.xlsx", sheet=1)
names(data)
data[,4:14] = lapply(data[,4:14], as.numeric)
table(data$sig_PRA_HC)
table(data$sig_RAA_HC)
dat=data%>%filter(sig_PRA_HC=="up in PRA, wilcox" ,sig_RAA_HC=="up in RAA, wilcox")%>%
  arrange(p_PRA_vs_HC)%>%data.frame()
rownames(dat)=dat$gene_position
dat1=dat[1:10,c("median_HC", "median_PRA", "median_RAA")]

col_fun = colorRamp2(c(-1,0, 1), c("#5D669F","white","#AF322F"))

top_annotation = HeatmapAnnotation(df=data.frame(group=c("Health","preRA","RA")),
                                   col = list(group=c(RA="#BF5960",Health="#6F99AD",preRA="#F9A363"))) 

pdf("Heatmap both_up_PRA_RAA_wilcox_top10.pdf", width = 5, height = 4)
Heatmap(t(scale(t(dat1))),col=col_fun,
        rect_gp = gpar(col="white", lwd=3),
        top_annotation = top_annotation,
        cluster_rows = F, cluster_columns = F,
        show_heatmap_legend = T,
        border = F, show_column_names = F,
        show_row_names = T, column_title = NULL)
dev.off()



