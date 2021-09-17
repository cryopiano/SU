rm(list= ls())
library(readxl)
setwd("D:/SU/Large Scale Challenges/DataLab1&2")
methane <- read_excel("Methane.xlsx")


t.test(methane$My_result,methane$My_friends_result, alternative = c("two.sided"), paired = TRUE, conf.level = 0.95)

trial <- read_excel("Methan_2_exp.xlsx")

var.test(trial$My_results,trial$My_friends_results, alternative = "less")

cherry <- read.csv("kyoto_dates_cherryblossom2021.csv")

cherry$date <- as.Date(with(cherry, paste(ï..Year, Month, Day,sep="-")), "%Y-%m-%d")
date = as.POSIXlt(cherry$date, format = "%Y-%m-%d")
cherry$n <- date$yday

pre <- subset(cherry, cherry$ï..Year < 1880)
post <- subset(cherry, cherry$ï..Year >= 1880)

t.test(post$n,pre$n, alternative = c("two.sided"), paired = FALSE, conf.level = 0.95)


hist(pre$n)
hist(post$n)

hist(cherry$n)