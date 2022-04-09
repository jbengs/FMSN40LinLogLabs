# # Lab2C
# 2.C(a). ####
# Is there a significant linear relationship between log(Pb) and time, now that we have
# taken the different levels in the regions into account?. Test H0: β1 = 0 against H1:
# β1 6= 0, using a suitable test and report the test-statistic, its degrees of freedom, the Pvalue and the conclusion.
library("ggplot2")
load("Data/PB_all.rda")
temp <- Pb_all$year - 1975
Pb_all["year"] <- temp
rm(temp)
summary(Pb_all)

Pb_all$region <- relevel(Pb_all$region, "Jamtland")
loglin.region <- lm(log(Pb) ~ year + region, data = Pb_all)
if(abs(summary(loglin.region)$coefficients[2,3]) > qt(1-0.05/2, 1225, lower.tail = TRUE)) {
  print("B1 is significant at 5%")
} else {
  print("B1 is not significant at 5%")
}


# 2.C(b). ####
# Are there any significant differences in the starting levels (1975) between the different
# regions? Test this using a suitable test, comparing the model from 2.A(b) with 2.B(d).
# Report the test statistic, its degrees of freedom, the P-value, and the conclusion.

model.reduced <- lm(log(Pb) ~ year, data = Pb_all)
model.full <- lm(log(Pb) ~ year + region, data = Pb_all)
(partialF <- anova(model.reduced, model.full))
if (partialF$F[2] > qf(0.05, 1229, 1225, lower.tail = FALSE)) {
  print("regions are significant, compared to having no region, by partial F-test")  
}

# 2.C(c). ####
# Add fitted lines, confidence intervals and prediction intervals to the plot(s)
# in 2.B(b) and 2.B(a). Do they seem reasonable? YES THEY DO

Pb.pred <- cbind(
  Pb_all,
  fit = predict(model.full),
  conf = predict(model.full, interval = "confidence"),
  pred = predict(model.full, interval = "prediction")
)
Pb.pred$conf.fit <- Pb.pred$pred.fit <- NULL

ggplot(data = Pb.pred , aes(x = year, y = log(Pb))) + 
  geom_point() + 
  facet_wrap(~region) +
  geom_line(aes(y = fit), color = "blue") + 
  geom_ribbon(aes(ymax = conf.upr, ymin = conf.lwr), alpha = 0.2) +
  geom_line(aes(y = pred.lwr), color = "orange", linetype = "dashed") +
  geom_line(aes(y = pred.upr), color = "orange", linetype = "dashed")

# 2.C(d). ####
# Calculate the residuals and plot them against the predicted log values. Also make a Q-Qplot. Problems? Compare with 2.A(e).
Pb.e <- cbind(
  Pb.pred,
  e = model.full$residuals
)
maxspan <- c(-max(abs(Pb.e$e)), max(abs(Pb.e$e)))

ggplot(data = Pb.e, aes(y=e, x = fit)) + 
  geom_point() + 
  geom_line(aes(y=0)) + 
  expand_limits(y = maxspan) 

ggplot(data = Pb.e, aes(sample = e)) + 
  geom_qq (size = 1) + 
  geom_qq_line(size = 0.5)

# 2.C(e). ####
# Redo the residual plots separately for each region by adding facet_wrap(~ region).
# Did adding the regions to the model solve the problems revealed in 2.A(f )?
ggplot(data = Pb.e, aes(y=e, x = fit)) + 
  geom_point() + 
  geom_line(aes(y=0)) + 
  expand_limits(y = maxspan) + 
  facet_wrap(~region)

ggplot(data = Pb.e, aes(sample = e)) + 
  geom_qq (size = 1) + 
  geom_qq_line(size = 0.5) +
  facet_wrap(~region)
