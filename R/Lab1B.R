#Lab 1 b - log-transformation
#addative change in X (add one year) should give relative change in y. Log-lin
library(ggplot2)
load("data/Pb_Jamtland.rda")
temp <- Pb_Jamtland$year - 1975
Pb_Jamtland["year"] <- temp
rm(temp)

#1.B(b). Plot y = log(Pb) against x = I(year-1975).
#Does this seem like a linear relationship? YES
ggplot(data = Pb_Jamtland, aes(x = year, y = log(Pb))) + geom_point()

# 1.B(c). Fit this log-transformed linear model and calculate the β-estimates,
#their standard errors and 95 % confidence intervals.
# 1 - get the model using lm
# 2 - create a complete dataframe with original data, the model and intervals
# 3 - remove duplicates
loglin <- lm(log(Pb) ~ year, data = Pb_Jamtland)

(estimates <- cbind(
  beta = loglin$coefficients,
  confint(loglin)
))

loglin.summary <- summary(loglin)
coefficients <- loglin.summary$coefficients
stderror <- coefficients["Std. Error"]

loglin.dataset <- cbind(
  Pb_Jamtland,
  fit = predict(loglin),
  conf = predict(loglin, interval = "confidence"),
  pred = predict(loglin, interval = "predict")
)

loglin.dataset$conf.fit <- NULL
loglin.dataset$pred.fit <- NULL


