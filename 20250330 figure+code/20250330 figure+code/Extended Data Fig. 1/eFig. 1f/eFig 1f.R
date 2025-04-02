rm(list = ls())
library(GGally)
library(devtools)
library(rstatix)

data1<-data.table::fread("eFig 1f.txt")
names(data1)
data = data1[,c("Protein IDs","LFQ intensity 0-RA-HM20250127","LFQ intensity 1-RA-HM20250203","LFQ intensity 2-RA-HM20250205","LFQ intensity 3-RA-HM20250206","LFQ intensity 4-RA-HM20250206","LFQ intensity 5-RA-HM20250207","LFQ intensity 6-RA-HM20250207","LFQ intensity 7-RA-HM20250207")]%>%
  filter(substring(data1$`Protein IDs`,1,4)!="REV_" & substring(data1$`Protein IDs`,1,4)!="CON_")
data[data==0]=NA
data[,2:9]=lapply(data[,2:9], log2)

p <- ggpairs(data[,-3],columns = 2:8, 
             upper = list(continuous = "cor", 
                          method = "pearson",
                          na ="na"),
             lower = list(continuous = "smooth",
                          na ="na"),
             diag = list(continuous = "densityDiag", 
                         na = "naDiag"))+
  theme_bw()+
  theme(panel.grid=element_blank())
p
ggsave("eFig 1f.pdf", width = 5.5, height = 5.5)
