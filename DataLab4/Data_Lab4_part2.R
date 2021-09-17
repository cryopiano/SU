rm(list= ls())
rm(century)
library(readxl)
setwd("D:/SU/Large Scale Challenges/DataLab4")

cherry <- read.csv("kyoto_dates_cherryblossom2021.csv")
View(cherry)

cherry$date <- as.Date(with(cherry, paste(ï..Year, Month, Day,sep="-")), "%Y-%m-%d")
date = as.POSIXlt(cherry$date, format = "%Y-%m-%d")
cherry$n <- date$yday

for(i in 1:13) {
  i
  for(j in 0:99) {
    group_number <- sprintf("%02d", as.numeric(i))
    cherry$group[100*i + j - 99] = group_number
  }
}

attach(cherry)
boxplot(n~group, col=rainbow(13), xlab="century from 800 (group)", ylab="date", main="Date of cherry blossum vs Century (from 800)")

anova_blossom <- aov(n~group)        
summary(anova_blossom)

TukeyHSD(anova_blossom, conf.level = 0.95)

summary(n, group)
tapply(n, group, summary)
