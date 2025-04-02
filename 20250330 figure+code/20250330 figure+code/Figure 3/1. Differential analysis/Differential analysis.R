setwd("H:/20250330 figure+code/Figure 3/1. Differential analysis")

library(dplyr)
library(ggplot2)
library(ggpubr)


########################################### Female
rm(list=ls())
data1=read.csv("log2 rb clinical 331_579_checked.csv", header = T)
data1[,c(12:ncol(data1))] = lapply(data1[,c(12:ncol(data1))], as.numeric)
table(data1$Sex)
data=data1%>%filter(Sex=="Female")
G1=data[data$Group=="Health"|data$Group=="preRA",]
G2=data[data$Group=="Health"|data$Group=="RA_A",]
G3=data[data$Group=="RA_A"|data$Group=="preRA",]
table(G1$group);table(G2$group);table(G3$group)

result=NULL
result=data.frame(ID=colnames(data)[12:ncol(data)],
                  p_preRA_Health_wilcox=sapply(G1[,12:(ncol(G1))], function(x){if("try-error"%in%class(try(wilcox.test(x~G1$Group)))){NA}else{wilcox.test(x~G1$Group)[["p.value"]]}}),
                  fc_preRA_Health=sapply(G1[,12:(ncol(G1))], function(x){median(x[G1$Group=="preRA"],na.rm=T)-median(x[G1$Group=="Health"],na.rm=T)}),
                  
                  p_RAA_Health_wilcox=sapply(G2[,12:(ncol(G2))], function(x){if("try-error"%in%class(try(wilcox.test(x~G2$Group)))){NA}else{wilcox.test(x~G2$Group)[["p.value"]]}}),
                  fc_RAA_Health=sapply(G2[,12:(ncol(G2))], function(x){median(x[G2$Group=="RA_A"],na.rm=T)-median(x[G2$Group=="Health"],na.rm=T)}),
                  
                  p_RAA_preRA_wilcox=sapply(G3[,12:(ncol(G3))], function(x){if("try-error"%in%class(try(wilcox.test(x~G3$Group)))){NA}else{wilcox.test(x~G3$Group)[["p.value"]]}}),
                  fc_RAA_preRA=sapply(G3[,12:(ncol(G3))], function(x){median(x[G3$Group=="RA_A"],na.rm=T)-median(x[G3$Group=="preRA"],na.rm=T)}),
                  
                  median_Health=sapply(data[data$Group=="Health",12:ncol(data)], function(x){median(x, na.rm=T)}),
                  median_preRA=sapply(data[data$Group=="preRA",12:ncol(data)], function(x){median(x, na.rm=T)}),
                  median_RA_A=sapply(data[data$Group=="RA_A",12:ncol(data)], function(x){median(x, na.rm=T)}))
result$preRA_Health=ifelse(result$p_preRA_Health_wilcox<0.05 & result$fc_preRA_Health<log2(1/1.2),"down", ifelse(result$p_preRA_Health_wilcox<0.05 & result$fc_preRA_Health>log2(1.2),"up", "ns")); table(result$preRA_Health)
result$RAA_Health=ifelse(result$p_RAA_Health_wilcox<0.05 & result$fc_RAA_Health<log2(1/1.2),"down", ifelse(result$p_RAA_Health_wilcox<0.05 & result$fc_RAA_Health>log2(1.2),"up", "ns")); table(result$RAA_Health)
result$RAA_preRA=ifelse(result$p_RAA_preRA_wilcox<0.05 & result$fc_RAA_preRA<log2(1/1.2),"down", ifelse(result$p_RAA_preRA_wilcox<0.05 & result$fc_RAA_preRA>log2(1.2),"up", "ns")); table(result$RAA_preRA)

colnames(result)=paste("F",colnames(result), sep="_")
write.csv(result,"wilcox result RA_A female.csv", row.names = F)



########################################### Male
rm(list=ls())
data1=read.csv("log2 rb clinical 331_579_checked.csv", header = T)
data1[,c(12:ncol(data1))] = lapply(data1[,c(12:ncol(data1))], as.numeric)
table(data1$Sex)
data=data1%>%filter(Sex=="Male")
G1=data[data$Group=="Health"|data$Group=="preRA",]
G2=data[data$Group=="Health"|data$Group=="RA_A",]
G3=data[data$Group=="RA_A"|data$Group=="preRA",]
table(G1$group);table(G2$group);table(G3$group)

result=NULL
result=data.frame(ID=colnames(data)[12:ncol(data)],
                  p_preRA_Health_wilcox=sapply(G1[,12:(ncol(G1))], function(x){if("try-error"%in%class(try(wilcox.test(x~G1$Group)))){NA}else{wilcox.test(x~G1$Group)[["p.value"]]}}),
                  fc_preRA_Health=sapply(G1[,12:(ncol(G1))], function(x){median(x[G1$Group=="preRA"],na.rm=T)-median(x[G1$Group=="Health"],na.rm=T)}),
                  
                  p_RAA_Health_wilcox=sapply(G2[,12:(ncol(G2))], function(x){if("try-error"%in%class(try(wilcox.test(x~G2$Group)))){NA}else{wilcox.test(x~G2$Group)[["p.value"]]}}),
                  fc_RAA_Health=sapply(G2[,12:(ncol(G2))], function(x){median(x[G2$Group=="RA_A"],na.rm=T)-median(x[G2$Group=="Health"],na.rm=T)}),
                  
                  p_RAA_preRA_wilcox=sapply(G3[,12:(ncol(G3))], function(x){if("try-error"%in%class(try(wilcox.test(x~G3$Group)))){NA}else{wilcox.test(x~G3$Group)[["p.value"]]}}),
                  fc_RAA_preRA=sapply(G3[,12:(ncol(G3))], function(x){median(x[G3$Group=="RA_A"],na.rm=T)-median(x[G3$Group=="preRA"],na.rm=T)}),
                  
                  median_Health=sapply(data[data$Group=="Health",12:ncol(data)], function(x){median(x, na.rm=T)}),
                  median_preRA=sapply(data[data$Group=="preRA",12:ncol(data)], function(x){median(x, na.rm=T)}),
                  median_RA_A=sapply(data[data$Group=="RA_A",12:ncol(data)], function(x){median(x, na.rm=T)}))
result$preRA_Health=ifelse(result$p_preRA_Health_wilcox<0.05 & result$fc_preRA_Health<log2(1/1.2),"down", ifelse(result$p_preRA_Health_wilcox<0.05 & result$fc_preRA_Health>log2(1.2),"up", "ns")); table(result$preRA_Health)
result$RAA_Health=ifelse(result$p_RAA_Health_wilcox<0.05 & result$fc_RAA_Health<log2(1/1.2),"down", ifelse(result$p_RAA_Health_wilcox<0.05 & result$fc_RAA_Health>log2(1.2),"up", "ns")); table(result$RAA_Health)
result$RAA_preRA=ifelse(result$p_RAA_preRA_wilcox<0.05 & result$fc_RAA_preRA<log2(1/1.2),"down", ifelse(result$p_RAA_preRA_wilcox<0.05 & result$fc_RAA_preRA>log2(1.2),"up", "ns")); table(result$RAA_preRA)

colnames(result)=paste("M",colnames(result), sep="_")
write.csv(result,"wilcox result RA_A male.csv", row.names = F)





########################################### all samples
rm(list=ls())
data=read.csv("log2 rb clinical 331_579_checked.csv", header = T)
data[,c(12:ncol(data))] = lapply(data[,c(12:ncol(data))], as.numeric)
table(data$Group)

i = 31
re = NULL
for(i in 12:ncol(data)){
  ID = gsub("\\.cit\\.", "(cit)", colnames(data)[i])
  
  # PRA vs HC
  d = data[data$Group=="Health" | data$Group=="preRA",c(2,i)] %>% na.omit(); colnames(d)[2]="variable"
  if(length(table(d$Group))<2){p_PRA_vs_HC = NA; fc_PRA_vs_HC = NA; n_PRA = nrow(d[d$Group=="preRA",]); n_HC = nrow(d[d$Group=="Health",])}else{
    n_PRA = table(d$Group)[2]; n_HC = table(d$Group)[1]
    
    if(n_PRA<5 | n_HC<5){p_PRA_vs_HC = NA; fc_PRA_vs_HC = NA}else{
      p_PRA_vs_HC = wilcox.test(d$variable~d$Group)[["p.value"]]
      fc_PRA_vs_HC = median(d$variable[d$Group=="preRA"],na.rm=T)-median(d$variable[d$Group=="Health"],na.rm=T)
    }
  }
  median_HC=median(d$variable[d$Group=="Health"],na.rm=T)
  median_PRA=median(d$variable[d$Group=="preRA"],na.rm=T)
  
  # RAA vs HC
  d = data[data$Group=="Health" | data$Group=="RA_A",c(2,i)] %>% na.omit(); colnames(d)[2]="variable"
  if(length(table(d$Group))<2){p_RAA_vs_HC = NA; fc_RAA_vs_HC = NA; n_RAA = nrow(d[d$Group=="RA_A",]); n_HC = nrow(d[d$Group=="Health",])}else{
    n_RAA = table(d$Group)[2]; n_HC = table(d$Group)[1]
    
    if(n_RAA<5 | n_HC<5){p_RAA_vs_HC = NA; fc_RAA_vs_HC = NA}else{
      p_RAA_vs_HC = wilcox.test(d$variable~d$Group)[["p.value"]]
      fc_RAA_vs_HC = median(d$variable[d$Group=="RA_A"],na.rm=T)-median(d$variable[d$Group=="Health"],na.rm=T)
    }
  }
  median_RAA=median(d$variable[d$Group=="RA_A"],na.rm=T)
  
  re = rbind(re, data.frame(i,ID, n_PRA, n_HC, n_RAA, p_PRA_vs_HC, fc_PRA_vs_HC, p_RAA_vs_HC, fc_RAA_vs_HC, median_HC, median_PRA, median_RAA))
}
re$sig_PRA_HC = ifelse(re$p_PRA_vs_HC<0.05 & re$fc_PRA_vs_HC>log2(1.2),"up in PRA, wilcox",ifelse(re$p_PRA_vs_HC<0.05 & re$fc_PRA_vs_HC<log2(1/1.2),"down in PRA, wilcox","ns"))
re$sig_RAA_HC = ifelse(re$p_RAA_vs_HC<0.05 & re$fc_RAA_vs_HC>log2(1.2),"up in RAA, wilcox",ifelse(re$p_RAA_vs_HC<0.05 & re$fc_RAA_vs_HC<log2(1/1.2),"down in RAA, wilcox","ns"))
table(re$sig_PRA_HC); table(re$sig_RAA_HC)

write.csv(re,"wilcox result atleast5.csv", row.names = F)
