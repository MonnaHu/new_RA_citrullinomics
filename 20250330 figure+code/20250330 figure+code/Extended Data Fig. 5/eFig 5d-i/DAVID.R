library(dplyr)
library(ggplot2)
library(ggpubr)

rm(list = ls())

# pos
pos = data.table::fread("DAVID pos.txt")%>%filter(PValue<0.05)
table(pos$Category)

bp = pos %>% filter(Category=="GOTERM_BP_DIRECT") %>% arrange(PValue)
bp = bp[1:5]%>%arrange(-PValue)%>%mutate(order=letters[1:5], group=rep("GOBP pos",5))

cc = pos %>% filter(Category=="GOTERM_CC_DIRECT") %>% arrange(PValue)
cc = cc[1:5]%>%arrange(Count)%>%mutate(order=letters[1:5], group=rep("GOCC pos",5))

mf = pos %>% filter(Category=="GOTERM_MF_DIRECT") %>% arrange(PValue)
mf = mf[1:5]%>%arrange(Count)%>%mutate(order=letters[1:5], group=rep("GOMF pos",5))


# neg
neg = data.table::fread("DAVID neg.txt")%>%filter(PValue<0.05)

bp1 = neg %>% filter(Category=="GOTERM_BP_DIRECT") %>% arrange(PValue)
bp1 = bp1[1:5]%>%arrange(Count)%>%mutate(order=letters[1:5], group=rep("GOBP neg",5))

cc1 = neg %>% filter(Category=="GOTERM_CC_DIRECT") %>% arrange(PValue)
cc1 = cc1[1:5]%>%arrange(Count)%>%mutate(order=letters[1:5], group=rep("GOCC neg",5))

mf1 = neg %>% filter(Category=="GOTERM_MF_DIRECT") %>% arrange(PValue)
mf1 = mf1[1:5]%>%arrange(Count)%>%mutate(order=letters[1:5], group=rep("GOMF neg",5))



data = rbind(mf, mf1, bp, bp1, cc, cc1)
data$Description = gsub(".*\\~","",data$Term)%>%stringr::str_to_title() 
data$y = paste(data$order, data$Description)

group = unique(data$group)


p_list <- lapply(group, function(x){
  ggplot(data[data$group==x,], aes(Count, y, fill=-log10(PValue)))+
    geom_bar(stat="identity", width = .6, color="transparent")+
    scale_fill_gradient(high="#FCA305", low="#A0CEE2")+
    scale_x_continuous(expand = c(0,0))+
    #scale_y_discrete(label=data$Description[data$group==x])+
    theme_minimal()+
    theme(axis.line = element_line(color="black", linewidth = .7),
          axis.ticks = element_line(color="black", linewidth = .7))+
    ggtitle(x)
})

pdf("eFig 5d-i.pdf", width = 18, height = 6)
do.call(gridExtra::grid.arrange,c(p_list, ncol=3))
dev.off()

