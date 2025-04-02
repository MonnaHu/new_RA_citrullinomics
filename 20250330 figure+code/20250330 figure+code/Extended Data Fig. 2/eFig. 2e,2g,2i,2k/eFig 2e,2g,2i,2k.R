library(dplyr)
library(ggplot2)
library(ggpubr)

rm(list = ls())
data=data.table::fread("up in PRA.txt")%>%arrange(PValue) %>% filter(PValue<0.05, Category=="GOTERM_BP_DIRECT")
dat=data[1:10,]%>%arrange(Count)%>%mutate(order=letters[1:10]) %>% na.omit()
names(dat)

ggplot(dat, aes(Count, order, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks = c(3,5,7,9))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,2,4))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("PRA_up_only GOBP.pdf", width = 4.5, height = 4)

dat$order1 = paste(dat$order, dat$Term)
ggplot(dat, aes(Count, order1, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks = c(3,5,7,9))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,2,4))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("PRA_up_only GOBP text.pdf", width = 7.5, height = 3)


rm(list = ls())
data=data.table::fread("up in RAA.txt")%>%arrange(PValue) %>% filter(PValue<0.05, Category=="GOTERM_BP_DIRECT")
dat=data[1:10,]%>%arrange(Count)%>%mutate(order=letters[1:10]) %>% na.omit()
names(dat)

ggplot(dat, aes(Count, order, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks = c(5,15,25,35))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,5,10,15,20,25))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("RAA_up_only GOBP.pdf", width = 4.5, height = 4)

dat$order1 = paste(dat$order, dat$Term)
ggplot(dat, aes(Count, order1, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks = c(5,15,25,35))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,5,10,15,20,25))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("RAA_up_only GOBP text.pdf", width = 7.5, height = 3)



rm(list = ls())
data=data.table::fread("both_up.txt")%>%arrange(PValue) %>% filter(PValue<0.05, Category=="GOTERM_BP_DIRECT")
dat=data[1:10,]%>%arrange(Count)%>%mutate(order=letters[1:10]) %>% na.omit()
names(dat)

ggplot(dat, aes(Count, order, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks = c(3,5,7,9))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,2,4,6,8,10))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("both_up GOBP.pdf", width = 4.5, height = 4)

dat$order1 = paste(dat$order, dat$Term)
ggplot(dat, aes(Count, order1, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks = c(3,5,7,9))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,2,4,6,8,10))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("both_up GOBP text.pdf", width = 7.5, height = 3)



rm(list = ls())
data=data.table::fread("down in PRA.txt")%>%arrange(PValue) %>% filter(PValue<0.05, Category=="GOTERM_BP_DIRECT")
d1=data[1:5,]%>%arrange(Count)%>%mutate(order=letters[1:5])
names(d1)

data=data.table::fread("both_down.txt")%>%arrange(PValue) %>% filter(PValue<0.05, Category=="GOTERM_BP_DIRECT")
d2=data[1:5,]%>%arrange(Count)%>%mutate(order=letters[1:5])
names(d2)

dat = rbind(d1%>%mutate(type="down in PRA"),d2%>%mutate(type="Xboth_down GOBP"))
dat$order=paste(dat$type, dat$order)

ggplot(dat, aes(Count, order, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks=c(0,1,2,3,4,5))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,2,4))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("PRA_down_only GOBP & both_down GOBP.pdf", width = 5.5, height = 4)

dat$order1 = paste(dat$order, dat$Term)
ggplot(dat, aes(Count, order1, fill=-log10(PValue)))+
  geom_bar(stat="identity", width = .6, color="transparent")+
  scale_size(range=c(2,5))+
  scale_fill_gradient(high="#FCA305", low="#A0CEE2", breaks=c(0,1,2,3,4,5))+
  scale_x_continuous(expand = c(0,0), breaks = c(0,2,4))+
  theme_minimal()+
  theme(axis.line = element_line(linewidth=.8, color="black"),
        axis.ticks = element_line(linewidth=.8, color="black"),
        axis.ticks.length = unit(1.25,"mm"))
ggsave("PRA_down_only GOBP & both_down GOBP text.pdf", width = 10, height = 3)




