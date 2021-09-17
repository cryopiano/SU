rm(list= ls())
setwd("D:/SU/Large Scale Challenges/DataLab1&2")
library(readxl)
data <- read_excel("StatLab1.xls")

attach(data)

plot(EndTime, Temp_degC, main = "Temperature recording")
plot(EndTime, Pressure_hPa, main = "Pressure recording")
plot(EndTime, RH_percent, main = "Relative humidity recording")
plot(EndTime, WindSpeed_m_s, main = "Wind speed recording")
plot(EndTime, WindDir_deg, main = "Wind direction recording")
plot(EndTime, Precip_mm, main = "Precipitations recording")
plot(EndTime, Bsp_1_m, main = "Scattering coefficient recording")
plot(EndTime, N_cnc_cc, main = "Particule concentration recording")

hist(Temp_degC, main = "Temperature recording")
hist(Pressure_hPa, main = "Pressure recording")
hist(RH_percent, main = "Relative humidity recording")    #might be normally distributed
hist(WindSpeed_m_s, main = "Wind speed recording")
hist(WindDir_deg, main = "Wind direction recording")  #may be normally distributed
hist(Precip_mm, main = "Precipitations recording")    #not normally distributed
hist(Bsp_1_m, main = "Scattering coefficient recording")
hist(N_cnc_cc, main = "Particule concentration recording")

boxplot(Temp_degC, main = "Temperature recording")
boxplot(Pressure_hPa, main = "Pressure recording")
boxplot(RH_percent, main = "Relative humidity recording")
boxplot(WindSpeed_m_s, main = "Wind speed recording")
boxplot(WindDir_deg, main = "Wind direction recording")
boxplot(Precip_mm, main = "Precipitations recording")
boxplot(Bsp_1_m, main = "Scattering coefficient recording")
boxplot(N_cnc_cc, main = "Particule concentration recording")


ks.test(RH_percent, pnorm, mean(RH_percent), sd(RH_percent))
shapiro.test(RH_percent)

ks.test(WindDir_deg, pnorm, mean(WindDir_deg), sd(WindDir_deg))
shapiro.test(WindDir_deg)

ks.test(Precip_mm, pnorm, mean(Precip_mm), sd(Precip_mm))
shapiro.test(WindDir_deg)


mode <- function(a) {
  uniqv <- unique(a)
  uniqv[which.max(tabulate(match(a, uniqv)))]
}

interval <- function(b) {
  m <- mean(b)
  error <- qnorm(0.975)*sd(b)/sqrt(length(b))
  left <- m-error
  right <- m+error
  interval <- paste(deparse(substitute(left)), "<", "Mean", "<", deparse(substitute(right)), sep = " ")
} 

statistics <- function(u) {
  result <- matrix(1:10, ncol = 2)
  result[1,1] = "Mean ="
  result[1,2] = mean(u)
  result[2,1] = "Median ="
  result[2,2] = median(u)
  result[3,1] = "Mode ="
  result[3,2] = mode(u)
  result[4,1] = "Standard deviation ="
  result[4,2] = sd(u)
  result[5,1] = "95% confidence interval ="
  result[5,2] = interval(u)
  print(deparse(substitute(u)))
  tabresult <- data.frame(Operator = result[1:5,1], Result = result[1:5,2])
  print(tabresult)
}

statistics(Temp_degC)
statistics(Pressure_hPa)
statistics(RH_percent)
statistics(WindSpeed_m_s)
statistics(WindDir_deg)
statistics(Precip_mm)
statistics(Bsp_1_m)
statistics(N_cnc_cc)

median <- apply(df, 2, median)
