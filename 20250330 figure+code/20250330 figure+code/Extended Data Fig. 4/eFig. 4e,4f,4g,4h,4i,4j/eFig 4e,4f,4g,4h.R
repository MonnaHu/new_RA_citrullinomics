library(ggplot2)
library(dplyr)
library(ggpubr)
library(scales)

rm(list=ls())
d1 = read.csv("Cohort1.csv")
d2 = read.csv("Cohort2.csv")

# MTX+LEF
d1_1 = d1 %>% filter(Drugs.HM=="MTXLEF" & c(Responder.HM=="Y"|Responder.HM=="N"))
d2_1 = d2 %>% filter(csDMARDs=="MTX+LEF" & c(Responder.HM=="Y"|Responder.HM=="N"))
df1 = as.data.frame(table(d1_1$Gender, d1_1$Responder.HM)); sum(df1$Freq)
df2 = as.data.frame(table(d2_1$Gender, d2_1$Responder.HM)); sum(df2$Freq)
df = rbind(df1 %>% mutate(dataset = "dataset 1"), df2 %>% mutate(dataset = "dataset 2"))
df
ggplot(df, aes(Var1,Freq,fill=Var2))+
  geom_bar(stat="identity",position = position_dodge(width = .9),width = .5, alpha=1)+
  scale_fill_manual(values = c("#737aac","#af322f"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,60), breaks = c(0,20,40,60))+
  geom_text(aes(label=Freq), position = position_dodge(width = .9))+
  facet_wrap(.~dataset, nrow=1)+
  theme_classic()+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("barplot gender&response of MTXLEF.pdf", width = 4, height = 3)


d = rbind(d1_1[,c("Responder.HM","Age")] %>% mutate(dataset = "dataset 1"), d2_1[,c("Responder.HM","Age")] %>% mutate(dataset = "dataset 2"))
table(d$dataset)

ggplot(d, aes(dataset,as.numeric(Age),fill=Responder.HM,color=Responder.HM))+
  geom_violin(alpha=.3, color="black",position = position_dodge(width = .8), width=.8)+
  geom_boxplot(width=.3, color="black",position = position_dodge(width = .8), outlier.colour = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = .15, dodge.width = .8), size=1)+
  scale_color_manual(values = c("#737aac","#af322f"))+
  scale_fill_manual(values = c("#737aac","#af322f"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,25,50,75,100))+
  stat_compare_means(method = "wilcox.test", label="..p.format..",label.y =90)+
  theme_classic()+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("violin age&response of MTXLEF.pdf", width = 4.5, height = 3)

d1_1[,9:13] = lapply(d1_1[,9:13],as.numeric)
d2_1[,7:13] = lapply(d2_1[,7:13],as.numeric)
d1_1[,9:13] = lapply(d1_1[,9:13],scale)
d2_1[,7:13] = lapply(d2_1[,7:13],scale)
dd = rbind(d1_1[,c("Responder.HM","DAS28.CRP.HM","TJC","SJC","CRP")] %>% mutate(dataset = "dataset 1"), d2_1[,c("Responder.HM","DAS28.CRP.HM","TJC","SJC","CRP")] %>% mutate(dataset = "dataset 2"))
table(dd$dataset)
ddd = reshape2::melt(dd, id.vars=c("Responder.HM","dataset"), measure.vars=2:5)
ggplot(ddd, aes(variable,as.numeric(value),fill=Responder.HM,color=Responder.HM))+
  #geom_violin(alpha=.3, color="black",position = position_dodge(width = .8), width=.8)+
  geom_boxplot(width=.5, color="black",position = position_dodge(width = .8), outlier.colour = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = .25, dodge.width = .8), size=1)+
  scale_color_manual(values = c("#737aac","#af322f"))+
  scale_fill_manual(values = c("white","white"))+
  scale_y_continuous(expand = c(0,0),limits = c(-2.5,6))+
  stat_compare_means(method = "wilcox.test", label="p.signif",label.y =5)+
  facet_wrap(.~dataset, nrow=1)+
  theme_classic()+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("boxplot 4cli&response of MTXLEF.pdf", width = 6, height = 3.5)




#############################################################################################
# MTX+HCQ
d1_1 = d1 %>% filter(Drugs.HM=="MTXHCQ" & c(Responder.HM=="Y"|Responder.HM=="N"))
d2_1 = d2 %>% filter(csDMARDs=="MTX+HCQ" & c(Responder.HM=="Y"|Responder.HM=="N"))
df1 = as.data.frame(table(d1_1$Gender, d1_1$Responder.HM)); sum(df1$Freq)
df2 = as.data.frame(table(d2_1$Gender, d2_1$Responder.HM)); sum(df2$Freq)
df = rbind(df1 %>% mutate(dataset = "dataset 1"), df2 %>% mutate(dataset = "dataset 2"))
df
ggplot(df, aes(Var1,Freq,fill=Var2))+
  geom_bar(stat="identity",position = position_dodge(width = .9),width = .5, alpha=1)+
  scale_fill_manual(values = c("#737aac","#af322f"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,60), breaks = c(0,20,40,60))+
  geom_text(aes(label=Freq), position = position_dodge(width = .9))+
  facet_wrap(.~dataset, nrow=1)+
  theme_classic()+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("barplot gender&response of MTXHCQ.pdf", width = 4, height = 3)

d = rbind(d1_1[,c("Responder.HM","Age")] %>% mutate(dataset = "dataset 1"), d2_1[,c("Responder.HM","Age")] %>% mutate(dataset = "dataset 2"))
table(d$dataset)

ggplot(d, aes(dataset,as.numeric(Age),fill=Responder.HM,color=Responder.HM))+
  geom_violin(alpha=.3, color="black",position = position_dodge(width = .8), width=.8)+
  geom_boxplot(width=.3, color="black",position = position_dodge(width = .8), outlier.colour = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = .15, dodge.width = .8), size=1)+
  scale_color_manual(values = c("#737aac","#af322f"))+
  scale_fill_manual(values = c("#737aac","#af322f"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,100), breaks = c(0,25,50,75,100))+
  stat_compare_means(method = "wilcox.test", label="..p.format..",label.y =90)+
  theme_classic()+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("violin age&response of MTXHCQ.pdf", width = 4.5, height = 3)


d1_1[,9:13] = lapply(d1_1[,9:13],as.numeric)
d2_1[,7:13] = lapply(d2_1[,7:13],as.numeric)
d1_1[,9:13] = lapply(d1_1[,9:13],scale)
d2_1[,7:13] = lapply(d2_1[,7:13],scale)
dd = rbind(d1_1[,c("Responder.HM","DAS28.CRP.HM","TJC","SJC","CRP")] %>% mutate(dataset = "dataset 1"), d2_1[,c("Responder.HM","DAS28.CRP.HM","TJC","SJC","CRP")] %>% mutate(dataset = "dataset 2"))
table(dd$dataset)
ddd = reshape2::melt(dd, id.vars=c("Responder.HM","dataset"), measure.vars=2:5)
ggplot(ddd, aes(variable,as.numeric(value),fill=Responder.HM,color=Responder.HM))+
  #geom_violin(alpha=.3, color="black",position = position_dodge(width = .8), width=.8)+
  geom_boxplot(width=.5, color="black",position = position_dodge(width = .8), outlier.colour = NA)+
  geom_jitter(position = position_jitterdodge(jitter.width = .25, dodge.width = .8), size=1)+
  scale_color_manual(values = c("#737aac","#af322f"))+
  scale_fill_manual(values = c("white","white"))+
  scale_y_continuous(expand = c(0,0),limits = c(-2.5,6))+
  stat_compare_means(method = "wilcox.test", label="p.signif",label.y =5)+
  facet_wrap(.~dataset, nrow=1)+
  theme_classic()+
  theme(axis.text = element_text(color="black"),
        strip.background = element_blank(),
        axis.text.x = element_text(angle=90),
        axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("boxplot 4cli&response of MTXHCQ.pdf", width = 6, height = 3.5)
