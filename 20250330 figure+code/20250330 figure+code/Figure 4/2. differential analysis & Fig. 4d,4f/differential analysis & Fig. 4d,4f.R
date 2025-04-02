setwd("H:/20250330 figure+code/Figure 4/2. differential analysis & Fig. 4d,4f")
library(dplyr)
library(ggplot2)

rm(list = ls())
data=read.csv("H:/20250330 figure+code/Figure 3/1. Differential analysis/log2 rb clinical 331_579_checked.csv", header = F,row.names = NULL)
colnames(data)=data[1,];data=data[-1,];data[,7:ncol(data)] = lapply(data[,7:ncol(data)], as.numeric)
table(data$Group)
table(data$`Responder.HM`)

table(data$`Responder.HM`, data$group)

dat=data[c(data$Group=="RA_A"|data$Group=="RA_B") & c(data$`Responder.HM`=="N"|data$`Responder.HM`=="Y") & c(data$`Drugs.HM`=="MTXLEF"),]
dat=dat[!is.na(dat$`Drugs.HM`),]

result=data.frame(ID=colnames(dat)[12:ncol(dat)],
                  n_Y=table(dat$`Responder.HM`)[["Y"]]-colSums(is.na(dat[dat$`Responder.HM`=="Y",12:ncol(dat)])),
                  n_N=table(dat$`Responder.HM`)[["N"]]-colSums(is.na(dat[dat$`Responder.HM`=="N",12:ncol(dat)])),
                  p_YN=sapply(dat[,12:ncol(dat)],function(x){if("try-error"%in%class(try(wilcox.test(x~dat$`Responder.HM`), silent=T))){NA}else{wilcox.test(x~dat$`Responder.HM`)[["p.value"]]}}),
                  fc_YN_mean=sapply(dat[,12:ncol(dat)],function(x){mean(x[dat$`Responder.HM`=="Y"],na.rm=T)-mean(x[dat$`Responder.HM`=="N"],na.rm=T)}),
                  mean_Y=sapply(dat[,12:ncol(dat)], function(x){mean(x[dat$`Responder.HM`=="Y"], na.rm=T)}),
                  mean_N=sapply(dat[,12:ncol(dat)], function(x){mean(x[dat$`Responder`=="N"], na.rm=T)}))
result$color_YN_mean=ifelse(result$p_YN<0.05 & result$fc_YN_mean>log2(1.1),"up",
                            ifelse(result$p_YN<0.05 & result$fc_YN_mean<log2(1/1.1),"down","other")); table(result$color_YN_mean)

write.csv(result,"response wilcox result  MTXLEF.csv", row.names = F)




dat=data[c(data$Group=="RA_A"|data$Group=="RA_B") & c(data$`Responder.HM`=="N"|data$`Responder.HM`=="Y") & c(data$`Drugs.HM`=="MTXHCQ"),]
dat=dat[!is.na(dat$`Drugs.HM`),]

result=data.frame(ID=colnames(dat)[12:ncol(dat)],
                  n_Y=table(dat$`Responder.HM`)[["Y"]]-colSums(is.na(dat[dat$`Responder.HM`=="Y",12:ncol(dat)])),
                  n_N=table(dat$`Responder.HM`)[["N"]]-colSums(is.na(dat[dat$`Responder.HM`=="N",12:ncol(dat)])),
                  p_YN=sapply(dat[,12:ncol(dat)],function(x){if("try-error"%in%class(try(wilcox.test(x~dat$`Responder.HM`), silent=T))){NA}else{wilcox.test(x~dat$`Responder.HM`)[["p.value"]]}}),
                  fc_YN_mean=sapply(dat[,12:ncol(dat)],function(x){mean(x[dat$`Responder.HM`=="Y"],na.rm=T)-mean(x[dat$`Responder.HM`=="N"],na.rm=T)}),
                  mean_Y=sapply(dat[,12:ncol(dat)], function(x){mean(x[dat$`Responder.HM`=="Y"], na.rm=T)}),
                  mean_N=sapply(dat[,12:ncol(dat)], function(x){mean(x[dat$`Responder`=="N"], na.rm=T)}))
result$color_YN_mean=ifelse(result$p_YN<0.05 & result$fc_YN_mean>log2(1.1),"up",
                            ifelse(result$p_YN<0.05 & result$fc_YN_mean<log2(1/1.1),"down","other")); table(result$color_YN_mean)

write.csv(result,"response wilcox result  MTXHCQ.csv", row.names = F)



# checked the Gene_position
result = read.csv("response wilcox result  MTXLEF checked.csv")

colnames(result)
result$label=ifelse(result$color_YN_mean=="up"|result$color_YN_mean=="down",result$Gene_position,NA)
ggplot(result[result$n_Y>=5 & result$n_N>=5, ], aes(fc_YN_mean,-log10(p_YN),color=color_YN_mean))+
  geom_point()+
  geom_vline(xintercept = c(log2(1.1), log2(1/1.1)), linetype=2)+
  geom_hline(yintercept = -log10(0.05), linetype=2)+
  geom_text(aes(label=label),nudge_x=0.1,nudge_y=.1,size=2)+
  scale_color_manual(values=c("#5D669F","#BBBBBB","#AF322F"),name="")+
  theme_test()+
  labs(x="Log2(Y/N)", y="-Log10(p value)",title = "")+
  theme(axis.text = element_text(color="black",size=12))
ggsave("Fig 4d.pdf", width = 3.5, height = 3)



result = read.csv("response wilcox result  MTXHCQ checked.csv")

result$label=ifelse(result$color_YN_mean=="up"|result$color_YN_mean=="down",result$Gene_position,NA)
ggplot(result[result$n_Y>=5 & result$n_N>=5, ], aes(fc_YN_mean,-log10(p_YN),color=color_YN_mean))+
  geom_point()+
  geom_vline(xintercept = c(log2(1.1), log2(1/1.1)), linetype=2)+
  geom_hline(yintercept = -log10(0.05), linetype=2)+
  geom_text(aes(label=label),nudge_x=0.1,nudge_y=.1,size=2)+
  scale_color_manual(values=c("#5D669F","#BBBBBB","#AF322F"),name="")+
  theme_test()+
  labs(x="Log2(Y/N)", y="-Log10(p value)",title = "")+
  theme(axis.text = element_text(color="black",size=12))
ggsave("Fig 4f.pdf", width = 3.5, height = 3)
