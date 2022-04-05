# Lab2B -Add location to the model
# 2B(a) ####
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


# 2B(b) - plot model ####
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
  


# 2.B(c). ####
# Fit a model using lm(log(Pb) ~ I(year - 1975) + region, data = Pb_all)
# Which location was used as reference? Is this a good idea?
# Answer: ÖREBRO was used as it is the first one. Not good as Örebro has the least number of observations
(summary(Pb_all))
loglin.region <- lm(log(Pb) ~ year + region, data = Pb_all)
(summary(loglin.region)$coefficients)


# 2.B(d). ####
# The reference category should always be a large category. Change the region variable so
# that [mzq]= Jamtland will be used as reference instead.
Pb_all$region <- relevel(Pb_all$region, "Jamtland")
# Then refit the model in 2.B(c).
loglin.region <- lm(log(Pb) ~ year + region, data = Pb_all)
(exp(summary(loglin.region)$coefficients))
   
# How high was the average concentration of lead in region [mzq] = Jamtland in 1975, with CI, according to themodel?
#ANSWER:  The exponential of the intercept plus the betafactor for that region. But now Jamtland is reference, so it is incorporated in B0 (the interecept)
(exp(confint(loglin.region)))
#How fast is the rate of decrease in lead concentration, according to this model? ANSWER: exp(B1) = exp(Betayear)

# 2.B(e). ####
#Use the model to calculate a 95 % confidence interval and a 95 % prediction interval for
#the lead concentration (mg/kg) in region [mzq] = Jamtland in the year [mzq. = 2014

Jamtland.2014 <- data.frame(year = c(2014-1975), region = "Jamtland")
Jamtland.2014.pred <- cbind(
  Jamtland.2014,
  fit = exp(predict(loglin.region, Jamtland.2014)),
  conf = exp(predict(loglin.region, Jamtland.2014, interval = "confidence")),
  pred = exp(predict(loglin.region, Jamtland.2014, interval = "prediction"))
)
Jamtland.2014.pred$conf.fit <- Jamtland.2014.pred$pred.fit <- NULL
(Jamtland.2014.pred)



# 2.B(f ). ####
#Estimate the ratio between the expected Pb-level in Orebro and the level in Jamtland for any given year.
#Answer: The Betaorebro-parameter

# 2.B(g). ####
#Orebro does not have any observations for 1975. Use the model to estimate the expected ¨
# Pb-level in Orebro in 1975, together with a 95 % confidence interval.
Orebro.1975 <- data.frame(year = c(1975-1975), region = "Orebro")
Orebro.pred.1975 <- cbind(
  Orebro.1975,
  fit = exp(predict(loglin.region, Orebro.1975)),
  conf = exp(predict(loglin.region, Orebro.1975, interval = "confidence")),
  pred = exp(predict(loglin.region, Orebro.1975, interval = "prediction"))
)
Orebro.pred.1975$conf.fit <- Orebro.pred.1975$pred.fit <- NULL
(Orebro.pred.1975)

# 2.B(h). ####
#Use the model to estimate the expected Pb-level in Orebro in the year [ ¨ mzq], together
# with a 95 % confidence interval.
Orebro.2014 <- data.frame(year = c(2014-1975), region = "Orebro")
Orebro.pred.2014 <- cbind(
  Orebro.2014,
  fit = exp(predict(loglin.region, Orebro.2014)),
  conf = exp(predict(loglin.region, Orebro.2014, interval = "confidence")),
  pred = exp(predict(loglin.region, Orebro.2014, interval = "prediction"))
)
Orebro.pred.2014$conf.fit <- Orebro.pred.2014$pred.fit <- NULL
(Orebro.pred.2014)