library(dplyr)
library(ggplot2)
library(ggridges)


rm(list=ls())
elisa = readxl::read_xlsx("eFig 1o.xlsx", sheet=1)
omics = readxl::read_xlsx("eFig 1o.xlsx", sheet=2)
names(elisa); names(omics)
colnames(elisa)[1:4]=c("sample","AGER","VCNA","OLFM4")
colnames(omics)[1:5]=c("sample","s","AGER","VCNA","OLFM4")
data = rbind(elisa[,1:4] %>% mutate(set="elisa"), omics[,c(1,3:5)] %>% mutate(set="Aomics"))
dat = reshape2::melt(data, id.vars=c("sample","set"))
dat$value = as.numeric(dat$value)
dat$g = paste(dat$variable,dat$set)

ggplot(na.omit(dat), aes(value,variable,fill=variable))+
  geom_density_ridges()+
  facet_wrap(.~set, nrow=1,scales = "free_x")+
  scale_fill_manual(values = c("#3d7dae","#d72a26","#eac450"))+
  theme_classic()+
  theme(strip.background = element_blank(),legend.position = "none",
        axis.line = element_line(color="black"),
        axis.ticks = element_line(color="black"))
ggsave("eFig 1o.pdf", width = 4.5, height = 3)
