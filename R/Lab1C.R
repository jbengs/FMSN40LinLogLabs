#Lab1c

library(ggplot2)
load("data/Pb_Jamtland.rda")
temp <- Pb_Jamtland$year - 1975
Pb_Jamtland["year"] <- temp
rm(temp)


#### Calculations ####
#  log-transformed linear model
loglin <- lm(log(Pb) ~ year, data = Pb_Jamtland)

alpha <- cbind(
  fit = exp(loglin$coefficients[1]),
  lwr = exp(confint(loglin)[1,1]),
  upr = exp(confint(loglin)[1,2])
)
b <- cbind(
  fit = exp(loglin$coefficients[2]),
  lwr = exp(confint(loglin)[2,1]),
  upr = exp(confint(loglin)[2,2])
)
(rbind(alpha,b)) 
