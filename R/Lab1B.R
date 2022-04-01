#Lab 1 b - log-transformation
#additive change in X (add one year) should give relative change in y. Log-lin
library(ggplot2)
load("data/Pb_Jamtland.rda")
temp <- Pb_Jamtland$year - 1975
Pb_Jamtland["year"] <- temp
rm(temp)

#1.B(b). Plot y = log(Pb) against x = I(year-1975).
#Does this seem like a linear relationship? YES
ggplot(data = Pb_Jamtland, aes(x = year, y = log(Pb))) + geom_point()

#### Calculations ####
# 1.B(c). Fit this log-transformed linear model and calculate the β-estimates,
#their standard errors and 95 % confidence intervals.
loglin <- lm(log(Pb) ~ year, data = Pb_Jamtland)
# To extract std.error (which is not Std.deviation), we get the second column
# of the coefficient matrix, which comes from summary 
loglin.summary <- summary(loglin)
coefficients <- loglin.summary$coefficients
stderror <- coefficients[, 2]
(estimates <- cbind(
  beta = loglin$coefficients,
  confint(loglin),
  stderror
))

#### Plot calculations for entire model ####
# 1 - get the model using lm
# 2 - create a complete dataframe WITH original data, the model and intervals
# 3 - remove duplicates
loglin.dataset <- cbind(
  Pb_Jamtland,
  fit = predict(loglin),
  conf = predict(loglin, interval = "confidence"),
  pred = predict(loglin, interval = "predict")
)

loglin.dataset$conf.fit <- NULL
loglin.dataset$pred.fit <- NULL

#### Plot of entire model ####
#geom_ribbon gives a shaded area. It wants an upper and lower. opacity is alpha.


ggplot(data = loglin.dataset, aes(x = year, y = log(Pb))) +
  geom_point() +
  geom_line( aes(y = fit), color = "blue", size = 1) +
  geom_ribbon( aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  geom_line( aes(y = pred.lwr), color ="red", linetype = "dashed", size = 1) +
  geom_line( aes(y = pred.upr), color = "orange", linetype = "dotted", size = 2) +
  labs(caption = "fitted line and 95% conf. and pred. intervals")



#### Plot calculations for specific prediction in the future (2014) ####

x.2014 <- data.frame(year = c(2014-1975))
loglin.2014 <- cbind(
  x.2014,
  fit = predict(loglin, x.2014),
  conf = predict(loglin, x.2014, interval = "confidence"),
  pred = predict(loglin, x.2014,interval = "prediction")
)
loglin.2014$conf.fit <- loglin.2014$pred.fit <- NULL
(loglin.2014)
#### Residual Calculation ###
#Add the column of residuals to the precious dataset, and name the column "e"
loglin.dataset <- cbind(
  loglin.dataset,
  e = loglin$residuals
)
#### Residual Plotting ####
e.max <- max(abs(loglin.dataset$e)) 
e.span <- c(-e.max, e.max)

#Residuals against Y-hat
ggplot(data = loglin.dataset, aes(x = fit, y = e)) + geom_point() + geom_line(y = 0) + geom_smooth() +
  ylab("residuals") +
  xlab("Y-hat")

#Residuals Q-Q-plot
ggplot(data = loglin.dataset, aes(sample = e)) + 
  geom_qq() + 
  geom_qq_line() + 
  labs(title = "Normal Q-Q-plot of the residuals")
