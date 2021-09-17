rm(list= ls())
library(readxl)
setwd("D:/SU/Large Scale Challenges/DataLab3&4")

cherry <- read.csv("kyoto_dates_cherryblossom2021.csv")
View(cherry)

cherry$date <- as.Date(with(cherry, paste(ï..Year, Month, Day,sep="-")), "%Y-%m-%d")
date = as.POSIXlt(cherry$date, format = "%Y-%m-%d")
cherry$n <- date$yday

pre <- subset(cherry, cherry$ï..Year < 1880)
post <- subset(cherry, cherry$ï..Year >= 1880)


plot(pre$n ~ pre$ï..Year, xlab="Calendar year", ylab="Time of full bloom (days)", main = "Time of full bloom versus Calendar year Before 1880")
abline(lm(pre$n ~ pre$ï..Year))

plot(post$n ~ post$ï..Year, xlab="Calendar year", ylab="Time of full bloom (days)", main = "Time of full bloom versus Calendar year After 1880")
abline(lm(post$n ~ post$ï..Year))

bloommodelpre <- lm(pre$n ~ pre$ï..Year)
summary(bloommodelpre)$r.squared
summary(bloommodelpre)$coefficients
confint(bloommodelpre)
cor.test(pre$n, pre$ï..Year, method = c("pearson"))

bloommodelpost <- lm(post$n ~ post$ï..Year)
summary(bloommodelpost)$coefficients
confint(bloommodelpost)  
cor.test(post$n, post$ï..Year, method = c("pearson"))
summary(bloommodelpost)$r.squared

###plot regression plus intervals
#pre 1880
library(ggplot2)
pre <- na.omit(pre) #delete NaN
temp_var<-predict(bloommodelpre, interval="prediction")
new_df <- cbind(pre, temp_var)
ggplot(new_df, aes(x=ï..Year, y=n))+geom_point()+geom_smooth(method=lm, se=TRUE)+geom_line(aes(y=lwr))+geom_line(aes(y=upr))+geom_line(aes(y=upr))+xlab("Caleandar year")+ylab("Date of full bloom (days)")+ggtitle('Time of full bloom versus Calendar year before 1880') +theme(plot.title = element_text(hjust = 0.5))

#post 1880
post <- na.omit(post) #delete NaN
temp_var<-predict(bloommodelpost, interval="prediction")
new_df <- cbind(post, temp_var)
ggplot(new_df, aes(x=ï..Year, y=n))+geom_point()+geom_smooth(method=lm, se=TRUE)+geom_line(aes(y=lwr))+geom_line(aes(y=upr))+geom_line(aes(y=upr))+xlab("Caleandar year")+ylab("Date of full bloom (days)")+ggtitle('Time of full bloom versus Calendar year after 1880') +theme(plot.title = element_text(hjust = 0.5))
