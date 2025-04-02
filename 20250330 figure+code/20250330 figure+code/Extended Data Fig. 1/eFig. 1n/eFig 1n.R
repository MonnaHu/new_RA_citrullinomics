library(dplyr)
library(ggplot2)

rm(list=ls())
p1 = read.csv("eFig 1n.csv")%>%
  filter(unique>=2, reporter>0)
names(p1)
table(p1$reporter)
p1$value[p1$value==0] = NA


ggplot(na.omit(p1), aes(value)) +
  geom_histogram(aes(y = ..density..), fill="#ffc0cb", bins=50, alpha=1) + 
  geom_density(color = "black", size = 0.5) +                           
  geom_vline(xintercept = median(p1$value[p1$protein=="Q6UX06"], na.rm = T), linetype=2, color="black")+
  geom_vline(xintercept = median(p1$value[p1$protein=="Q15109"], na.rm = T), linetype=2, color="black")+
  geom_vline(xintercept = median(p1$value[p1$protein=="P13611"], na.rm = T), linetype=2, color="black")+
  theme_classic() +
  scale_x_continuous(limits = c(0, 0.2 * 10^7)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,0.8))+
  scale_x_log10() +
  theme(
    axis.text = element_text(color="black"),
    axis.line = element_line(linewidth=.8, color="black"),
    axis.ticks = element_line(linewidth=.8, color="black"),
    axis.ticks.length = unit(1.25, "mm")
  )

ggsave("density plot of all peptides.pdf", width = 3, height =3 )



