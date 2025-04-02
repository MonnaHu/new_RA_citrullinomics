library(dplyr)
library(ggplot2)

rm(list=ls())
dat1=readxl::read_xlsx("eFig 3b.xlsx", sheet=1)
data = dat1[,10:17] %>% data.frame(); data[,] = lapply(data[,], as.numeric)
range(data, na.rm=T)
rownames(data)=dat1$ID_PA

data[data==0]=NA
data$ratio=log2(apply(data[,5:8],1, mean, na.rm=T) / apply(data[,1:4],1, mean, na.rm=T))
data$ID=gsub("\\(.*","",dat1$Gene_position)
data$n_OA=4-rowSums(is.na(data[,1:4])); range(data$n_OA)
data$n_RA=4-rowSums(is.na(data[,5:8])); range(data$n_RA)
data$unique=rep(1,nrow(data))

dat=data[data$n_OA >= 2 & data$n_RA >= 2, c("ID", "ratio", "unique")] %>% group_by(ID) %>% summarise(log2sum=sum(ratio), n=sum(unique))
dat$type=ifelse(dat$log2sum > 0.5, "up", ifelse(dat$log2sum<(-0.5), "down", "ns")); table(dat$type)
write.csv(dat,"The citrullination state change (∆Ps).csv", row.names = F)


ggplot(dat, aes(n, log2sum, color=type))+
  geom_point(size=3)+
  theme_classic()+
  geom_hline(yintercept = c(0.5,-0.5), linetype=2, color="#CCCCCC")+
  labs(x="No. of citrullinated peptides", y="ΔCs value")+
  scale_color_manual(values = c('#154399', "#BBBBBB","#B81C23"))+
  scale_x_sqrt()+
  theme(axis.text = element_text(color="black"), axis.ticks = element_line(color="black", linewidth = .7),
        axis.line = element_line(linewidth = .7), axis.ticks.length = unit(1.25,"mm"))
ggsave("The citrullination state change (∆Cs).pdf", width = 4.25, height = 3)

table(dat$type)

df=read.csv("The citrullination state change (∆Ps).csv")
ggplot(df, aes(n, log2sum, color=type))+
  geom_point(size=3)+
  theme_classic()+
  geom_text(aes(label=ID), color="black")+
  geom_hline(yintercept = c(0.5,-0.5), linetype=2, color="#CCCCCC")+
  labs(x="No. of citrullinated peptides", y="ΔCs value")+
  scale_color_manual(values = c('#154399', "#BBBBBB","#B81C23"))+
  theme(axis.text = element_text(color="black"), axis.ticks = element_line(color="black", linewidth = .7),
        axis.line = element_line(linewidth = .7), axis.ticks.length = unit(1.25,"mm"))
ggsave("∆Cs text.pdf", width = 7, height = 6)
