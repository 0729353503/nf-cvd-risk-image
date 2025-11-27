#!/usr/bin/Rscript

## MEMORY NYASHA MHEMBERE
## University of Edinburgh - School of Informatics
## Msc Data Science - Dissertation (DSTI Student)
## s1772433
## 2020

library(ggplot2)
library(ggthemes)
library(gridExtra)
args <- commandArgs(trailingOnly = TRUE)
cvd_risk <- read.csv(args[1])

##OUTLIERS

bmi <- 
  ggplot(cvd_risk, aes(x='', y = bmi)) +
  geom_boxplot(fill = '#00AFBB') + 
  xlab('BMI') + ylab('') + coord_flip() + 
  theme_few() 

ldl<- 
  ggplot(cvd_risk, aes(x='', y = ldl)) +
  geom_boxplot(fill = '#00AFBB') + 
  coord_flip() +
  theme_few()  + 
  xlab('LDL')  + ylab('')

out<- arrangeGrob(bmi ,ldl,  nrow=2)
ggsave(plot=out, width=6, height=4, dpi=300, filename =args[2])
dev.off()
