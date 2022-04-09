#Lab3 A and B
library("ggplot2")
load("Data/PB_all.rda")
temp <- Pb_all$year - 1975
Pb_all["year"] <- temp
rm(temp)
summary(Pb_all)

# 3. A(a) calculations ####
# bad model (linlin) and good model (loglin)
linmod <- lm(Pb ~ year, data = Pb_all)
logmod <- lm(log(Pb) ~ year, data = Pb_all)

# Calculations
# Plot the leverage against year and add horizontal lines at 1/n and 2(p+1)/n.

#Lin

linmod.pred <- cbind(
  Pb_all,
  fit = predict(linmod),
  conf = predict(linmod, interval = "confidence"),
  pred = predict(linmod, interval = "prediction"),
  v = influence(linmod)$hat
)
linmod.pred$conf.fit <- linmod.pred$pred.fit <- NULL 

##Log
logmod.pred <- cbind(
  Pb_all,
  fit = predict(logmod),
  conf = predict(logmod, interval = "confidence"),
  pred = predict(logmod, interval = "prediction"),
  v = influence(logmod)$hat
)
logmod.pred$conf.fit <- logmod.pred$pred.fit <- NULL

#n is number of observations
n = nrow(Pb_all)
# p is number of parameters, exuding the intercept. So p+1 = all coefficents
p = length(linmod$coefficients)

# 3. A(a) Plots ####
# Linmod
ggplot(data = linmod.pred, aes(y = v, x = year)) +
  geom_jitter(width = 1) + 
  geom_line(aes(y = 1/n), color = "blue") +
  geom_line(aes(y = 2*(p)/n), color = "red") +
  expand_limits(y = 0) +
  labs(title = "Ligmod: all leverage vs all observations") +
  theme(text = element_text(size = 18))

  
# Logmod
ggplot(data = logmod.pred, aes(y = v, x = year)) +
  geom_jitter(width = 1) + 
  geom_line(aes(y = 1/n), color = "blue") +
  geom_line(aes(y = 2*(p)/n), color = "red") +
  expand_limits(y = 0) +
  labs(title = "Logmod: all leverage vs all observations") +
  theme(text = element_text(size = 18))


# Report the mean value of the variable year, where the leverage has its minimum. Hint: use mean() on the year variable.
#MEANYEAR
(mean(Pb_all$year) + 1975)
leverage.outliers <- c(1975-1975, 1980-1975)

# CALCULATIONS FOR  b/c/d/e ####
lrmodel <- lm(log(Pb) ~ year + region, data = Pb_all)
lrmodel.pred <- cbind(
  Pb_all,
  fit = predict(lrmodel),
  conf = predict(lrmodel, interval = "confidence"),
  pred = predict(lrmodel, interval = "prediction"),
  v = influence(lrmodel)$hat,
  r = rstudent(lrmodel),
  D = cooks.distance(lrmodel)
)
lrmodel.pred$conf.fit <- lrmodel.pred$pred.fit <- NULL

#n is number of observations
n = nrow(lrmodel.pred)
# p is number of parameters, exuding the intercept. So p+1 = all coefficents
p = length(lrmodel$coefficients)
# r.outliers is residuals with abs greater than 3.
r.outliers <- which(abs(lrmodel.pred$r) > 3)
# leverage.outliers is from visual inspection in 3. A(a)
leverage.outliers <- c(1975-1975, 1980-1975)
#COOKS DISTANCE
freedom1 <- length(lrmodel$coefficients) #equals p+1
freedom2 <- lrmodel$df.residual # equals n(p+1) which equals n minus freedom1
alpha = 1-0.5
fquantile <- qf(alpha,freedom1,freedom2) #removed from plot because no Cooks distances were that large.
n = nrow(lrmodel.pred)
cooks.outliers <- which(lrmodel.pred$D > 4/n)

# 3. A(b) plot ####
ggplot(data = lrmodel.pred, aes(y = v, x = year, color = region)) + 
  geom_jitter(width = 1) + 
  geom_line(aes(y = 1/n), color = "blue") +
  geom_line(aes(y = 2*(p)/n), color = "red") +
  expand_limits(y = 0) +
  labs(title = "Logmod + region: all leverage vs all observations") +
  theme(text = element_text(size = 18))
# TRUE: The more observations there are in a region, the lower the leverage of the individual observations.
    
# 3. A(c) plot ####
# How many unpleasantly large residuals are there? Hint: use which(). Answer: 5, because number 1153 is hidden behind 1145.

ggplot(data = lrmodel.pred, aes(y = r, x = fit)) +
  geom_point() +
  geom_hline(yintercept = c(-2,0,2), color = "blue") + 
  geom_hline(yintercept = c(-3,3), color = "red", linetype = "dashed") +
  geom_point(data = lrmodel.pred[r.outliers, ], color = "red", size = 4, shape = 24) +
  geom_smooth() +
  labs(title = "studentized residuals (r) vs Y-hat") +
  theme(text = element_text(size = 18))

# 3. A(d) r plot ####
# Redo the plot separately for each region using +facet_wrap(~region). Are there any
# trends in the residuals in any of the regions? What might that indicate?

ggplot(data = lrmodel.pred, aes(y = r, x = fit)) +
  facet_wrap(~region) +
  geom_point() +
  geom_hline(yintercept = c(-2,0,2), color = "blue") + 
  geom_hline(yintercept = c(-3,3), color = "red", linetype = "dashed") +
  geom_point(data = lrmodel.pred[r.outliers, ], color = "red", size = 4, shape = 24) +
  geom_smooth() +
  labs(title = "studentized residuals (r) vs Y-hat by region") +
  theme(text = element_text(size = 18))

# 3. A(e) square root of r plot ####
ggplot(data = lrmodel.pred, aes(y = sqrt(abs(r)), x = fit)) +
  facet_wrap(~region) +
  geom_point() +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2)), color = "blue") + 
  geom_hline(yintercept = sqrt(3), color = "red", linetype = "dashed") +
  geom_point(data = lrmodel.pred[outliers, ], color = "red", size = 4, shape = 24) +
  expand_limits(y = 0) +
  geom_smooth() +
  labs(title = "root of studentized residuals (r) vs Y-hat by region") +
  theme(text = element_text(size = 18))


# 3. A(f) COOKS DISTANCE plot ####

ggplot(lrmodel.pred, aes(y = D, x = fit)) +
  facet_wrap(~region) +
  geom_point() +
  # geom_hline(yintercept = fquantile, color = "red") +
  geom_hline(yintercept = 4/n, linetype = "dashed", color = "blue") +
  expand_limits(y = 0) + 
  geom_point(data = lrmodel.pred[r.outliers, ], color = "red", size = 4, shape = 24) +
  geom_point(data = lrmodel.pred[leverage.outliers, ], color = "purple", size = 4, shape = 22) +
  geom_point(data = lrmodel.pred[cooks.outliers, ], color = "green", size = 4, shape = 21) +
  labs(title = "Logmod + region: cooks distande vs Y-hat") +
  theme(text = element_text(size = 18))

# 3. A(d) DFBETAS plot #### 
# Calculate DFBETAS and save the ones for the time variable "year". Plot them against time,
# separately for each location. Did the influential points in 3.A(f ) in Orebro have a large ¨
# influence on the estimate of the rate of decline? Did the one in Vastra Gotaland?
  
head(dfbetas(lrmodel))
lrmodel.pred$dfbetas.year <- dfbetas(lrmodel)[, "year"]

ggplot(data = lrmodel.pred, aes(y = dfbetas.year, x = year)) +
  facet_wrap(~region) +
  geom_point() +
  geom_point(data = lrmodel.pred[r.outliers, ], color = "red", size = 4, shape = 24) +
  geom_point(data = lrmodel.pred[leverage.outliers, ], color = "purple", size = 4, shape = 22) +
  geom_point(data = lrmodel.pred[cooks.outliers, ], color = "green", size = 4, shape = 21) +
  labs(title = "Logmod + region: dfbetas vs year") +
  theme(text = element_text(size = 18))

# 3. A(h) final plot ####
# plot log pb against year and hihlight outliers
ggplot(data = Pb_all, aes( y = log(Pb), x = year)) +
  facet_wrap(~region) +
  geom_point() +
  expand_limits(y = 0) + 
  geom_point(data = lrmodel.pred[r.outliers, ], color = "red", size = 4, shape = 24) +
  geom_point(data = lrmodel.pred[leverage.outliers, ], color = "purple", size = 4, shape = 22) +
  geom_point(data = lrmodel.pred[cooks.outliers, ], color = "green", size = 4, shape = 21) +
  labs(title = "Logmod + region: Pb vs year, with outliers") +
  theme(text = element_text(size = 18))

  
  
         
# LAB 3 B
# 3. B(a) model building ####
# log(Pb) ~ I(year - 1975)*region
model.0 <- lm(log(Pb) ~ year, data = Pb_all)
model.1 <- lm(log(Pb) ~ year + region, data = Pb_all)
model.2 <- lm(log(Pb) ~ year*region, data = Pb_all)
(sum.0 <- summary(model.0))
(sum.1 <- summary(model.1))
(sum.2 <- summary(model.2))
anova(model.1, model.2)
# Answer: Partial F used gives f-statistic 4.9616, with f1 = 4 and f2 = 1221. Reject H0. There are interactions.

# Calculate for all three models Do the measures agree on which model is best? Worst?
# For the model that is best according to AIC, how much of the variability in log-lead
# concentration does is explain?

# 3. B(b) calculations ####
collect.R2s <- data.frame(
  nr = seq(1,3),
  model = c("0. only year", "1. year + region", "2. year * region"),
  R2 = c(sum.0$r.squared,
         sum.1$r.squared,
         sum.2$r.squared),
  R2.adj = c(sum.0$adj.r.squared,
             sum.1$adj.r.squared,
             sum.2$adj.r.squared)
  )

collect.AIC <- data.frame(
  nr = seq(1,3),
  model = c("0. Only year", "1. year + region", "2. year * region"),
  AIC(model.0, model.1, model.2),
  BIC(model.0, model.1, model.2)
)

# 3. B(c) Plots ####
ggplot(collect.R2s, aes(model, R2)) +
  geom_point(size = 3) + 
  geom_point(aes(y = R2.adj), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = R2.adj), 
            color = "red", size = 1, linetype = "dashed") +
  geom_hline(yintercept = 1) +
  labs(caption = "R2 (black), R2-adj (red dashed)") +
  labs(title = "Cabbage: R2 and R2-adjusted") +
  ylab("R2 and R2-adj") +
  theme(text = element_text(size = 18))

ggplot(collect.AIC, aes(model, AIC)) +
  geom_point(size = 3) + 
  geom_point(aes(y = BIC), color = "red", size = 3) + 
  geom_line(aes(x = nr), size = 1) +
  geom_line(aes(x = nr, y = BIC), 
            color = "red", size = 1, linetype = "dashed") +
  labs(caption = "AIC (black), BIC (red dashed)") +
  labs(title = "Cabbage: AIC and BIC") +
  ylab("AIC and BIC") +
  theme(text = element_text(size = 18))
# Bic says that model 1 is better than model 2
# AIC says that model 2 is better than model 1
# R2 and R2 adjust says that model 2 is better than model 1
# R2 says that model 2 explains 81.3 % of the variability!