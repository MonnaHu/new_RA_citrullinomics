setwd("H:/20250330 figure+code/Figure 3/Fig 3a,3b")
library(ggplot2)

rm(list = ls())
result=read.csv("H:/20250330 figure+code/Figure 3/1. Differential analysis/wilcox result RA_A female.csv")
result1=read.csv("H:/20250330 figure+code/Figure 3/1. Differential analysis/wilcox result RA_A male.csv")
result=cbind(result,result1)

result$color=ifelse(result$F_preRA_Health=="up" & result$M_preRA_Health=="up","up",
                    ifelse(result$F_preRA_Health=="down" & result$M_preRA_Health=="down","down","ns")); table(result$color)

ggplot(result, aes(x=F_fc_preRA_Health, y=M_fc_preRA_Health, color=color))+
  geom_abline(intercept = 0, slope = 1, linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_vline(xintercept = c(log2(1.2),-log2(1.2)), linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_hline(yintercept = c(log2(1.2),-log2(1.2)), linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_point()+
  scale_color_manual(values = c("#5D669F","#BFBFBF","#AF322F"),name="")+
  theme_test()+
  theme(axis.line = element_blank(),
        panel.border = element_rect(linewidth=.8),
        axis.ticks = element_line(linewidth=.8),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("volcano preRA_Health F_M.pdf", width = 3.8, height = 3)

cor.test(result$F_fc_preRA_Health, result$M_fc_preRA_Health, method="spearman")



result$color=ifelse(result$F_RAA_Health=="up" & result$M_RAA_Health=="up","up",
                    ifelse(result$F_RAA_Health=="down" & result$M_RAA_Health=="down","down","ns")); table(result$color)

ggplot(result, aes(x=F_fc_RAA_Health, y=M_fc_RAA_Health, color=color))+
  geom_abline(intercept = 0, slope = 1, linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_vline(xintercept = c(log2(1.2),-log2(1.2)), linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_hline(yintercept = c(log2(1.2),-log2(1.2)), linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_point()+
  scale_color_manual(values = c("#5D669F","#BFBFBF","#AF322F"),name="")+
  theme_test()+
  theme(axis.line = element_blank(),
        panel.border = element_rect(linewidth=.8),
        axis.ticks = element_line(linewidth=.8),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("volcano RAA_HC F_M.pdf", width = 3.8, height = 3)

cor.test(result$F_fc_RAA_Health, result$M_fc_RAA_Health, method="spearman")
