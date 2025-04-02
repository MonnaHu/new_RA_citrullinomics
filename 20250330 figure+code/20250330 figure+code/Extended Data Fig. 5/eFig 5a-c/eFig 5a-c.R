library(dplyr)
library(ggseqlogo)
library(ggplot2)

rm(list = ls())
df = readxl::read_xlsx("bigru_3_64.xlsx", sheet=1) %>% 
  filter(nchar_8AApep==17)
df$seq17aa = gsub("X","R",df$seq17aa)
table(df$group)
df = df %>% arrange(pred)
dat=list(pos_all=df$seq17aa[df$group=="all_pos"],neg_all=df$seq17aa[df$group=="all_neg"],neg_top=df$seq17aa[1:209])

ggseqlogo(dat, seq_type = "aa", scale="free", method = "probability", nrow=4, stack_width=0.8)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  theme(axis.line.y=element_line(color="black"),
        axis.ticks.y=element_line(color="black"),
        axis.text.x = element_blank())
ggsave("eFig 5a-c.pdf", width = 4, height = 7)
























###########################################################################################
rm(list = ls())
df = readxl::read_xlsx("F:/RA/第一次投稿/审稿意见/3models_new(20250120为返修重新做的)/整理结果.xlsx", sheet=9) %>% filter(nchar_8AApep==17, `是否在阴性集中跟第一个sheet匹配`==0,`是否在阳性集中跟第一个sheet匹配`==0)

table(df$group)
dat=list(pos_all=df$`8AApep`[df$group=="Predicted antigenic citrullinated peptides"],neg_all=df$`8AApep`[df$group=="Predicted non-antigenic citrullinated peptides"])

ggseqlogo(dat, seq_type = "aa", scale="free", method = "probability", nrow=4, stack_width=0.8)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  theme(axis.line.y=element_line(color="black"),
        axis.ticks.y=element_line(color="black"),
        axis.text.x = element_blank())
ggsave("Fig 6c,6d 所有17aa肽段做出来的.pdf", width = 3.5, height = 5)




df = readxl::read_xlsx("F:/RA/第一次投稿/审稿意见/3models_new(20250120为返修重新做的)/整理结果.xlsx", sheet=8) %>% filter(nchar==17, `是否在阴性集中跟第一个sheet匹配`==0,`是否在阳性集中跟第一个sheet匹配`==0)
table(df$group)
table(df$result_0.8)
dat=list(pos_all=df$`8AApep`[df$result_0.8==1],neg_all=df$`8AApep`[df$result_0.8==0])

ggseqlogo(dat, seq_type = "aa", scale="free", method = "probability", nrow=4, stack_width=0.8)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))+
  theme(axis.line.y=element_line(linewidth = 0.7,color="black"),
        axis.ticks.y=element_line(linewidth = 0.7,color="black"),
        axis.ticks.length =  unit(1.25,"mm"),
        axis.text.x = element_blank())
ggsave("Fig 6c,6d.pdf", width = 3.5, height = 5)
