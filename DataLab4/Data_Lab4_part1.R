rm(list= ls())
library(readxl)
#rearrange the dataset before load in R
setwd("D:/SU/Large Scale Challenges/DataLab4")

data <- read_excel("Interlab.xlsx")
attach(data)

data$lab <- sprintf("%02d", as.numeric(lab))
print(lab)
boxplot(pah~lab, col=rainbow(4), main="PAHs vs Lab")
boxplot(pah~method, col=rainbow(5), main="PAHs vs Method")
anova <- aov(pah~lab+method)
summary(anova)


model_lab <- aov(pah ~ lab)
model_method <- aov(pah~method)
TukeyHSD(model_lab, conf.level = 0.95)
TukeyHSD(model_method, conf.level = 0.95)
TukeyHSD(anova, confi.level= 0.95)
