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

pb.2014 <- data.frame( year = c(2014-1975))

pb.pred <- cbind(
  pb.2014,
  fit = exp(predict(loglin, pb.2014)),
  conf = exp(predict(loglin, pb.2014,interval = "confidence")),
  pred = exp(predict(loglin, pb.2014, interval = "prediction"))
)
pb.pred$conf.fit <- pb.pred$pred.fit <- NULL
(pb.pred)
