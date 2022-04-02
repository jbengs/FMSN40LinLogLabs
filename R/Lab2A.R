#Lab2 - t,test, p-value, residual analysis by region using facet_wrap(~region) ####
library("ggplot2")
load("Data/PB_all.rda")
temp <- Pb_all$year - 1975
Pb_all["year"] <- temp
rm(temp)
summary(Pb_all)

#  Plot Pb against year using all the data. Does it look like an exponential decline might work? - NO
ggplot(data = Pb_all, aes( x = year, y = Pb)) +geom_point()

#Fit a linear model with y = log(Pb) and x = I(year - 1975) using all the data.
loglin <- lm(log(Pb) ~ year, data = Pb_all)

# t-test for year ####
# compare t-value with t-quantile:
# upper alpha/2-t-quantile with df = 26
(loglin.sum <- summary(loglin))
(loglin.sum$coefficients)
(tvalue <- loglin.sum$coefficients["year","t value"])
qt(1-0.05/2,1229)
if(abs(tvalue) > qt(1-0.05/2,1229)){
  print("observed t-statistic lies outside quantile at 5% confidence -> reject H0 by t-test")
  # abs(t-value) > quantile for students t -> unreasonable observation -> disregard H0 -> year is significant
  } else {
  print("observed t-statistic lies inside quantile at 5% confidence -> H0 holds by t-test")
}

# P-value for year ####
# calculate P-value:
# Alternative One: Get it from summary
loglin.sum$coefficients["year", "Pr(>|t|)"]
# Alternative 2, get it from pt-function 2*P(|t| > |tvalue|) to cover both tails:
(pvalue <- 2*pt(abs(tvalue), 1229, lower.tail = FALSE))
#Both are equal, and shallbe compared to alpha
if(pvalue < 0.05){
  print("P-value is lower than alpha -> unlikey observation -> disregard H0")
} else {
  print("P-value is greater than alpha -> Not unlikely observation -> H0 holds")
}
#Alternative 3: use confint

# Estimate for 2014 ####
pb.2014 <- data.frame(year = c(2014-1975))
loglin.2014 <- cbind(
  pb.2014,
  fit = exp(predict(loglin, pb.2014)),
  conf = exp(predict(loglin, pb.2014, interval = "confidence")),
  pred = exp(predict(loglin, pb.2014,interval = "prediction"))
)
loglin.2014$conf.fit <- loglin.2014$pred.fit <- NULL
(loglin.2014)


# Residual Analysis without region ####
pb.e <- cbind(
  Pb_all,
  fit = exp(predict(loglin)),
  conf = exp(predict(loglin, interval = "confidence")),
  pred = exp(predict(loglin, interval = "prediction")),
  e = summary(loglin)$residuals
)
pb.e.espan <- c(-max(abs(pb.e$e)), max(abs(pb.e$e)))

# Residual Plot, e against Y-hat
ggplot(data = pb.e, aes(y = e, x = fit)) + 
  geom_point() + 
  expand_limits(y = pb.e.espan) + 
  geom_line(y = 0) +
  geom_smooth(method = "loess")+
  ylab("Residuals") + 
  xlab("Y-hat")

#Residual plot, Q-Q-plot
ggplot(data = pb.e, aes(sample = e)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))


# Residual Analysis including region ####
# Residual Plot, e against Y-hat
ggplot(data = pb.e, aes(y = e, x = fit)) + 
  facet_wrap(~region) +
  geom_point() + 
  expand_limits(y = pb.e.espan) + 
  geom_line(y = 0) +
  geom_smooth(method = "loess")+
  ylab("Residuals") + 
  xlab("Y-hat")

#Residual plot, Q-Q-plot
ggplot(data = pb.e, aes(sample = e)) +
  facet_wrap(~region) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 18))
