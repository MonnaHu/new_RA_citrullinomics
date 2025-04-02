setwd("H:/20250330 figure+code/Figure 3/Fig. 3c,3e")
library(ggplot2)

result=read.csv("H:/20250330 figure+code/Figure 3/1. Differential analysis/wilcox result atleast5.csv", header = T,row.names = 1)

ggplot(result, aes(x=fc_PRA_vs_HC, y=-log10(p_PRA_vs_HC), color=sig_PRA_HC))+
  geom_point()+
  geom_hline(yintercept = -log10(0.05), linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_vline(xintercept = c(log2(1.2),-log2(1.2)), linetype=2,linewidth=.5, color="#BBBBBB")+
  scale_color_manual(values = c("#5D669F","#BFBFBF","#AF322F"))+
  theme_test()+
  theme(axis.line = element_blank(),
        panel.border = element_rect(linewidth=.8),
        axis.ticks = element_line(linewidth=.8),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("Fig 3c.pdf", width = 3.8, height = 3)

ggplot(result, aes(x=fc_RAA_vs_HC, y=-log10(p_RAA_vs_HC), color=sig_RAA_HC))+
  geom_point()+
  geom_hline(yintercept = -log10(0.05), linetype=2,linewidth=.5, color="#BBBBBB")+
  geom_vline(xintercept = c(log2(1.2),-log2(1.2)), linetype=2,linewidth=.5, color="#BBBBBB")+
  scale_color_manual(values = c("#5D669F","#BFBFBF","#AF322F"))+
  theme_test()+
  theme(axis.line = element_blank(),
        panel.border = element_rect(linewidth=.8),
        axis.ticks = element_line(linewidth=.8),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("Fig 3e.pdf", width = 3.8, height = 3)
