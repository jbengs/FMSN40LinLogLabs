#My first script file
load("data/Pb_Jamtland.rda")
summary(Pb_Jamtland)
head(Pb_Jamtland)
tail(Pb_Jamtland)

library(ggplot2)
#### inital model calculations usnig lm ####
#Plot pb against year lineary
ggplot(Pb_Jamtland, aes(x = year, y = Pb)) + geom_point()
#No, it is not linear, it is exponential decline. Seems natural! becuase decay is relative to levels.

#transformed model so that year starts at 0
ggplot(Pb_Jamtland, aes(x = I(year - 1975), y = Pb)) + geom_point()

#Fit model, addative transform.
linModTrans <- lm(Pb ~ I(year-1975), data = Pb_Jamtland)

#Get beta estimates, for B0 and B1 (intercept and yearly linear change)
#Get confidence intervals for beta estimates
linModTrans.summary <- summary(linModTrans)
(model.coefficients <- cbind(linModTrans.summary$coefficients, confint(linModTrans)))

#### 2014 single prediction ####
#get estimate for 2014, which is in the future.
#we have to construct a year vector and provide it to toe predict functions.
# We then get three versions of the fitted line! 
# Get rid of the extra ones by setting them to NULL:
lead.x0 <- data.frame(year = c(2014))
lead.estimatex0 <- cbind(lead.x0,
    fit = predict(linModTrans, lead.x0),
    conf = predict(linModTrans, lead.x0, interval = "confidence"),
    pred = predict(linModTrans, lead.x0, interval = "predict")
    )
lead.estimatex0$conf.fit <- lead.estimatex0$pred.fit <- NULL
head(lead.estimatex0)

#### plotting calculations of lead.pred dataset ####
#Get entire estimate line
(lead.pred = cbind(Pb_Jamtland,
  fit = predict(linModTrans),
  conf = predict(linModTrans, interval = "confidence"),
  pred = predict(linModTrans, interval = "predict"))
)
lead.pred$conf.fit <- lead.pred$pred.fit <- NULL

#### PLOT of lead.pred ####
# Fine tune the original plot with datapoints. Save it ass plot.data
# Note lead.pred contains both orignal data and all info on lines
# thanks to us using the original dataset in cbind above
(
  plot.data <-
    ggplot(lead.pred, aes(x = I(year - 1975), y = Pb))
  + geom_point()
  + xlab("year since 1975")
  + ylab("lead level")
  + labs(title = "Lead in natur, in Jamtland")
  + labs(caption = "original scale")
  + theme(text = element_text(size = 14))
)

# Add the lines to plot.data.
# Note: ggplot is only used once, now we just add stuff to the same plot
(
  plot.logdata <- plot.data
  + geom_line(aes(y = fit), color = "blue", size = 1)
  + geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) 
  + geom_line(aes(y = pred.lwr), color = "red", linetype = "dashed", size = 1)
  + geom_line(aes(y = pred.upr), color = "red", linetype = "dashed", size = 1)
  + labs(caption = "fitted line and 95% conf. and pred. intervals")
  )
  

#### Residual calculations ####
#Residuals of the model, saved as a column in lead.pred.
#the with of residuals, calculated for symetry in plotting
(lead.pred$e <- linModTrans$residuals)
(max.e <- max(abs(lead.pred$e)))
(max.span <- c(-max.e, max.e))

#Calculate the residuals and plot them against the predicted values. As visual aides, add
#a horizontal reference line at 0 and a moving average, +geom_smooth(). Any problems
#here? yes! Clear pattern, not random!
( plot.e1 <- ggplot(data = lead.pred, aes(y = e, x = I(year-1975)))
+ geom_point()
+ geom_hline(yintercept = 0)
+ expand_limits(y = max.span)
+ geom_smooth()
)

( plot.e2 <- ggplot(data = lead.pred, aes(sample = e))
  + geom_qq(size = 3)
  + geom_qq_line()
  + labs(tag = "C")
  + labs(title = "Normal Q-Q-plot of the residuals")
  + theme(text = element_text(size = 18))
)

# Histogram of the residuals:
( plot.histogram <- ggplot(data =lead.pred, aes(x = e))
  + geom_histogram(bins = 10)
  + xlab("Residuals")
  + labs(title = "Histogram of residuals")
  + theme(text = element_text(size = 18))
)

