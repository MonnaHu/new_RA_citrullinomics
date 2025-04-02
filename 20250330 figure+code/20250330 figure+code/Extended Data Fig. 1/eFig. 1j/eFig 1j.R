library(dplyr); library(ggplot2)

rm(list =ls())
d = readxl::read_xlsx("279 samples.xlsx", sheet=1)
d$ID = paste(d$batch,d$reporter)

d1 = read.csv("rep 1.csv")
d1$batch = gsub("\\_.*","",d1$batch)
d2 = read.csv("rep 2.csv") 
d1 = d1[d1$batch %in% d2$batch,] %>% filter(reporter!=0)
d2 = d2[d2$batch %in% d1$batch,] %>% filter(reporter!=0)

d1$value[d1$value==0]=NA; d2$value[d2$value==0]=NA
d1$value = log2(d1$value); d2$value = log2(d2$value)

d1$ID = paste(d1$batch, d1$reporter)
d2$ID = paste(d2$batch, d2$reporter)
d1 = d1[d1$ID %in% d$ID,] 
d2 = d2[d2$ID %in% d$ID,] 

d1_1 = reshape2::dcast(d1, protein ~ ID, value.var = "value")
d2_1 = reshape2::dcast(d2, protein ~ ID, value.var = "value")

d1_1 = d1_1[d1_1$protein %in% d2_1$protein,]
d2_1 = d2_1[d2_1$protein %in% d1_1$protein,]

table(d1_1$protein == d2_1$protein)
table(colnames(d1_1)==colnames(d2_1))

re = NULL
i = 1
for(i in 2:ncol(d1_1)){
  sample = colnames(d1_1)[i]
  d = cbind(d1_1[,i], d2_1[,i]) %>% data.frame() %>% na.omit(); colnames(d)=c("ID1","ID2")
  p = cor.test(d$ID1, d$ID2, method="spearman")[["p.value"]]
  cor = cor.test(d$ID1, d$ID2, method="spearman")[["estimate"]][["rho"]]
  re = rbind(re, data.frame(sample,p,cor))
}

ggplot(re, aes(cor))+
  geom_density(fill="#ffc0cb")+
  geom_vline(xintercept = median(re$cor), linetype=2)+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(limits = c(0,15), expand = c(0,0))+
  theme_classic()+
  theme(legend.position = "none",
        axis.line = element_line(linewidth = .7,color="black"),
        axis.ticks = element_line(linewidth = .7,color="black"),
        axis.ticks.length=unit(0.12, "cm"))
ggsave("eFig 1j.pdf", width = 3, height = 3)

