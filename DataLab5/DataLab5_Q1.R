rm(list= ls())

method <- data.frame(
  value = c("median", "k"),
  C_EFF = c(5,5), 
  D = c(1000,2), 
  C_NOAEL = c(1,10))

attach(method)
Q = (C_EFF[1]/D[1]*10)/C_NOAEL[1]

k_output = exp(((log(C_EFF[2]))^2 +  (log(C_NOAEL[2]))^2 + (log(D[2]))^2)^(0.5))

lower = Q/k_output
upper = Q*k_output
