setwd("H:/20250330 figure+code/Figure 4/1. Spearman analysis & Fig. 4a")
library(ggplot2)
library(dplyr)

rm(list = ls())
data=read.csv("H:/20250330 figure+code/Figure 3/1. Differential analysis/log2 rb clinical 331_579_checked.csv", header = F,row.names = NULL)
colnames(data)=data[1,];data=data[-1,];data[,7:ncol(data)] = lapply(data[,7:ncol(data)], as.numeric)
dat = data

result=NULL
result=data.frame(ID=colnames(dat)[12:ncol(dat)],
                  n_ABC=colSums(is.na(dat[,12:ncol(dat)])),
                  
                  p_DAS28=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$`DAS28.CRP.HM`, method="spearman")))){NA}else{cor.test(x,dat$`DAS28.CRP.HM`, method="spearman")[["p.value"]]}}),
                  rho_DAS28=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$`DAS28.CRP.HM`, method="spearman")))){NA}else{cor.test(x,dat$`DAS28.CRP.HM`, method="spearman")[["estimate"]][["rho"]]}}),
                  # 
                  p_SJC=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$SJC, method="spearman")))){NA}else{cor.test(x,dat$SJC, method="spearman")[["p.value"]]}}),
                  rho_SJC=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$SJC, method="spearman")))){NA}else{cor.test(x,dat$SJC, method="spearman")[["estimate"]][["rho"]]}}),
                  # 
                  p_TJC=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$TJC, method="spearman")))){NA}else{cor.test(x,dat$TJC, method="spearman")[["p.value"]]}}),
                  rho_TJC=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$TJC, method="spearman")))){NA}else{cor.test(x,dat$TJC, method="spearman")[["estimate"]][["rho"]]}}),
                  # 
                  p_CRP=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$CRP, method="spearman")))){NA}else{cor.test(x,dat$CRP, method="spearman")[["p.value"]]}}),
                  rho_CRP=sapply(dat[,12:ncol(dat)], function(x){if("try-error"%in%class(try(cor.test(x,dat$CRP, method="spearman")))){NA}else{cor.test(x,dat$CRP, method="spearman")[["estimate"]][["rho"]]}}))
result$sig_DAS28=ifelse(result$p_DAS28<0.05 & result$rho_DAS28<0,"neg", ifelse(result$p_DAS28<0.05 & result$rho_DAS28>0,"pos", "ns")); table(result$sig_DAS28)
result$sig_SJC=ifelse(result$p_SJC<0.05 & result$rho_SJC<0,"neg", ifelse(result$p_SJC<0.05 & result$rho_SJC>0,"pos", "ns")); table(result$sig_SJC)
result$sig_TJC=ifelse(result$p_TJC<0.05 & result$rho_TJC<0,"neg", ifelse(result$p_TJC<0.05 & result$rho_TJC>0,"pos", "ns")); table(result$sig_TJC)
result$sig_CRP=ifelse(result$p_CRP<0.05 & result$rho_CRP<0,"neg", ifelse(result$p_CRP<0.05 & result$rho_CRP>0,"pos", "ns")); table(result$sig_CRP)

write.csv(result,"spearman result.csv", row.names = F)


d=rbind(data.frame(table(result$sig_DAS28)),data.frame(table(result$sig_SJC)),data.frame(table(result$sig_TJC)),
          data.frame(table(result$sig_CRP)))
d$group=rep(c("1DAS28",'SJC',"TJC",'xCRP'), 
              c(length(table(result$sig_DAS28)),length(table(result$sig_SJC)),length(table(result$sig_TJC)),
                length(table(result$sig_CRP))))
library(ggplot2)

ggplot(d,aes(group,Freq,fill=Var1))+
  geom_bar(stat="identity", position = "stack", color="transparent", width = .6)+
  scale_fill_manual(values = c(pos="#AF322F", neg="#5D669F", ns="#BBBBBB"))+
  theme_test()+
  theme(axis.line = element_blank(),
        panel.border = element_rect(linewidth=.8),
        axis.ticks = element_line(linewidth=.8),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("Fig 4a.pdf", width = 3.5, height=3)

