# Lab2B -Add location to the model ####
library("ggplot2")
load("Data/PB_all.rda")
temp <- Pb_all$year - 1975
Pb_all["year"] <- temp
rm(temp)
summary(Pb_all)

# Plot the data again, as in 2.A(a), but separately for each region by adding facet_wrap
# the exponential decay holds accross all models
ggplot(data = Pb_all, aes( x = year, y = Pb)) +
  geom_point() +
  facet_wrap(~region)

#2B(b) - plot model ####
# Fit a linear model with y = log(Pb) and x = I(year - 1975) using all the data.
# Does this seem like a good idea for all the regions?
# YES it is still a good model,but region needs to be taken into account.
loglin <- lm(log(Pb) ~ year, data = Pb_all)

Pb.pred <- cbind(
  Pb_all,
  fit = predict(loglin),
  conf = predict(loglin, interval = "confidence"),
  pred = predict(loglin, interval = "prediction")
)
Pb.pred$conf.fit <- Pb.pred$pred.fit <- NULL

ggplot(data = Pb.pred , aes(x = year, y = log(Pb))) + 
  geom_point() + 
  facet_wrap(~region) +
  geom_line(aes(y = fit))
  


#2.B(c). Fit a model using lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
# Which location was used as reference? Is this a good idea?
  