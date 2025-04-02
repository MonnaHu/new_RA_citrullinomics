setwd("F:/RA/第一次投稿/审稿意见/Fig 3/TOP")
library(dplyr)
library(ggplot2)


rm(list = ls())
data=readxl::read_xlsx("eFig 2f,2h,2j.xlsx", sheet=1)
names(data)
data[,4:14] = lapply(data[,4:14], as.numeric)
table(data$sig_PRA_HC)
table(data$sig_RAA_HC)
dat=data%>%filter(sig_PRA_HC=="up in PRA, wilcox" ,sig_RAA_HC!="up in RAA, <5 >40%" , sig_RAA_HC!="up in RAA, wilcox")%>%
  arrange(p_PRA_vs_HC)%>%data.frame()
rownames(dat)=dat$gene_position

dat1=dat[1:10,]%>%mutate(order=letters[10:1])
names(dat1)
dat1$order = paste(dat1$order, dat1$gene_position)

ggplot(dat1, aes(-log10(p_PRA_vs_HC),order, fill=`fc_PRA_vs_HC`))+
  geom_bar(stat="identity", width=.01, color="black", fill="black")+
  geom_point(shape=21, size=5)+
  scale_size(breaks=c(16,19,21), range=c(2,5.5),name="")+
  scale_fill_gradient(high="#F9A363", low="#ffefe4",name="", breaks=c(0.5,1.0,1.5))+
  theme_minimal()+
  theme(axis.line.x = element_line(color="black"),
        axis.ticks.x = element_line(color="black"))
ggsave("point PRA_up_only_wilcox_top10.pdf", width = 4, height = 3)



rm(list = ls())
data=readxl::read_xlsx("eFig 2f,2h,2j.xlsx", sheet=1)
names(data)
data[,4:14] = lapply(data[,4:14], as.numeric)
table(data$sig_PRA_HC)
table(data$sig_RAA_HC)
dat=data%>%filter(sig_RAA_HC=="up in RAA, wilcox" ,sig_PRA_HC!="up in PRA, <5 >40%" , sig_PRA_HC!="up in PRA, wilcox")%>%
  arrange(p_RAA_vs_HC)%>%data.frame()
rownames(dat)=dat$gene_position

dat1=dat[1:10,]%>%mutate(order=letters[10:1])
names(dat1)
dat1$order = paste(dat1$order, dat1$gene_position)

ggplot(dat1, aes(-log10(p_RAA_vs_HC),order, fill=`fc_RAA_vs_HC`))+
  geom_bar(stat="identity", width=.01, color="black", fill="black")+
  geom_point(shape=21, size=5)+
  scale_size(breaks=c(16,19,21), range=c(2,5.5),name="")+
  scale_fill_gradient(high="#F9A363", low="#ffefe4",name="", breaks=c(0.3,0.35,0.4))+
  theme_minimal()+
  theme(axis.line.x = element_line(color="black"),
        axis.ticks.x = element_line(color="black"))
ggsave("point RAA_up_only_wilcox_top10.pdf", width = 4.3, height = 3)


rm(list = ls())
data=readxl::read_xlsx("eFig 2f,2h,2j.xlsx", sheet=1)
names(data)
data[,4:14] = lapply(data[,4:14], as.numeric)
table(data$sig_PRA_HC)
table(data$sig_RAA_HC)
dat=data%>%filter(sig_PRA_HC=="up in PRA, wilcox" ,sig_RAA_HC=="up in RAA, wilcox")%>%
  arrange(p_PRA_vs_HC)%>%data.frame()
rownames(dat)=dat$gene_position

dat1=dat[1:10,]%>%mutate(order=letters[10:1])
names(dat1)
dat1$order = paste(dat1$order, dat1$gene_position)

d_PRA = dat1[,c("order","p_PRA_vs_HC","fc_PRA_vs_HC")]; colnames(d_PRA)[2:3]=c("p","log2fc"); d_PRA$log2fc = -d_PRA$log2fc
d_RAA = dat1[,c("order","p_RAA_vs_HC","fc_RAA_vs_HC")]; colnames(d_RAA)[2:3]=c("p","log2fc")

dat2=rbind(d_PRA, d_RAA)

ggplot(dat2, aes(log2fc,order, fill=-log10(p)))+
  geom_bar(stat="identity", width=.01, color="black", fill="black")+
  geom_point(shape=21, size=5)+
  geom_vline(xintercept = 0,linetype=2)+
  scale_size(breaks=c(16,19,21), range=c(2,5.5),name="")+
  scale_fill_gradient(high="#AF322F", low="#f7ddd8",name="", breaks=c(15,25,35))+
  theme_minimal()+
  theme(axis.line.x = element_line(color="black"),
        axis.ticks.x = element_line(color="black"))
ggsave("point both_up_PRA_RAA_wilcox_top10.pdf", width = 4, height = 3)
