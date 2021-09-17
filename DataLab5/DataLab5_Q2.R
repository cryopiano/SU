rm(list= ls())
library(propagate)

eggs <- c(24,10)*10^-3
milk <- c(10,4)*10^-3
meat <- c(32,10)*10^-3
veg <- c(0.1,0.02)*10^-3

cons_eggs <- c(0.3,0.1)
cons_milk <- c(1.8,0.6)
cons_meat <- c(1.6,0.7)
cons_veg <- c(0.02,0.005)

data_bde <- cbind(eggs,milk,meat,veg,cons_eggs,cons_milk,cons_meat,cons_veg)

EXPR <- expression(eggs*cons_eggs+milk*cons_milk+meat*cons_meat+veg*cons_veg)

monte_carlo <- propagate(expr = EXPR, data = data_bde, verbose = TRUE, nsim = 1000000)
monte_carlo
plot(monte_carlo)

EFSA = 12
dnorm(EFSA, mean = monte_carlo$sim[1], sd = monte_carlo$sim[2], log = FALSE)
pnorm(EFSA, mean = monte_carlo$sim[1], sd = monte_carlo$sim[2], lower.tail = FALSE, log = FALSE)
