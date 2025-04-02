setwd("H:/20250330 figure+code/Figure 1/Fig. 1b")
library(dplyr)
library(ggplot2)
library(ggpubr)

rm(list = ls())
d = dir(pattern = ".csv", full.names = T)

i=1
rep = NULL
for(i in 1:3){
  x= read.csv(d[i])
  names(x)
  x1 = x%>% mutate(t3 = substring(x$Proteins,1,4))%>%filter(t3!="CON_" & t3!="REV_") %>% select(-t3)
  x2 = x1 %>% filter(!grepl("R\\(pa\\)_",x1$Modified.sequence))
  x3 = x2 %>% filter(grepl("PAD", x2$Modifications))
  x3$rep=i
  rep = rbind(rep,x3)
}

rep$`ratio3 <0.01` = ifelse(rep$Ratios3 < 0.01,"yes","no");table(rep$`ratio3 <0.01`)
rep$`ratio3 <0.008` = ifelse(rep$Ratios3 < 0.008,"yes","no");table(rep$`ratio3 <0.008`)
rep$`ratio3 <0.006` = ifelse(rep$Ratios3 < 0.006,"yes","no");table(rep$`ratio3 <0.006`)
rep$`ratio3 <0.004` = ifelse(rep$Ratios3 < 0.004,"yes","no");table(rep$`ratio3 <0.004`)
rep$`ratio3 <0.002` = ifelse(rep$Ratios3 < 0.002,"yes","no");table(rep$`ratio3 <0.002`)

names(rep)
rep1 = reshape2::melt(rep, id.vars=1:38, measure.vars=39:43)
df = as.data.frame(table(rep1$rep, rep1$variable))
df$yes = NA
for(i in 1:nrow(df)){
  df$yes[i] = nrow(rep1[rep1$rep==df$Var1[i] & rep1$variable==df$Var2[i] & rep1$value=="yes",])
}
df$prob = round(df$yes/df$Freq * 100,2)


ratio3 = df
ratio3$x = rep(as.factor(5:1), times = rep(3,5))

ggbarplot(ratio3, x = "x", y = "prob", width=.6,
          add = c("mean_se"), add.params = list(width=0.2),fill="#c1bcdc", color="black")+
  geom_jitter(width = .3, shape=21, fill="#45519e")+
  labs(x="cutoff")+
  scale_y_continuous(expand = c(0,0), limits = c(0,100))+
  theme(axis.line = element_line(color="black", linewidth = .7),
        axis.ticks = element_line(color="black", linewidth = .7),
        axis.ticks.length =  unit(1.25,"mm"))
ggsave("Fig 1b.pdf", width = 3.3, height = 3)

