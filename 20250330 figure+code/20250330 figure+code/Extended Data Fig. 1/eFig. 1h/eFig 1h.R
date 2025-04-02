rm(list = ls())
library(GGally)
library(devtools)
library(rstatix)

rm(list =ls())
data<-read.csv("eFig 1h.csv") %>% filter(reporter==0) 
colnames(data)[4]="IS"
data$batch = gsub("\\_.*","",data$batch)

data$IS[data$IS==0]=NA

data1 = reshape2::dcast(data, protein ~ batch, value.var = "IS")
ratio = colSums(data1[,2:ncol(data1)], na.rm=T)/mean(colSums(data1[,2:ncol(data1)], na.rm=T))
for(i in 2:6){data1[,i] = data1[,i]/ratio[i-1]}
data1[,2:6] = lapply(data1[,2:6], log10)

data1 =data1 %>% mutate(na=rowSums(is.na(data1[,2:ncol(data1)]))) %>% filter(na<5) %>% select(-na)

ggpairs(data1,columns = 2:6, 
        upper = list(continuous = "cor",
                     method = "pearson",
                     na ="na"),
        lower = list(continuous = "smooth",
                     na ="na"),
        diag = list(continuous = "densityDiag", 
                    na = "naDiag"))+
  theme_bw()+
  theme(panel.grid=element_blank())
ggsave("eFig 1h.pdf", width = 6, height = 6)





rm(list =ls())
data<-read.csv("F:/RA/第一次投稿/审稿意见/滑膜重新搜库/data clean step2/ProteinGroups REV_CON synovium.csv")
data$IS = as.numeric(data$IS)
data$IS[data$IS==0]=NA

data1 = reshape2::dcast(data, Proteins ~ batch, value.var = "IS")
ratio = colSums(data1[,2:ncol(data1)], na.rm=T)/mean(colSums(data1[,2:ncol(data1)], na.rm=T))
for(i in 2:6){data1[,i] = data1[,i]/ratio[i-1]}
data1[,2:6] = lapply(data1[,2:6], log2)

data1 =data1 %>% mutate(na=rowSums(is.na(data1[,2:ncol(data1)]))) %>% filter(na<5) %>% select(-na)

ggpairs(data1,columns = 2:6, # 选择需要绘图的变量所在列号
             upper = list(continuous = "cor", # 为各种数据类型设置显示形式，ggally中功能函数的后缀(ggally_cor())。
                          method = "pearson",
                          na ="na"),# 设置参数名称与成对数据类型名称一致。
             lower = list(continuous = "smooth",#ggally_smooth()
                          na ="na"),
             diag = list(continuous = "densityDiag", # 'densityDiag', 'barDiag', 'blankDiag'可选
                         na = "naDiag"))+
  theme_bw()+
  theme(panel.grid=element_blank())# 设置主题参数
ggsave("IS synovium.pdf", width = 6, height = 6)
