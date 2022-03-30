#My first script file
load("data/Pb_Jamtland.rda")
summary(Pb_Jamtland)
head(Pb_Jamtland)
tail(Pb_Jamtland)

library(ggplot2)

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

#get estimate for 2014, which is in the future.
#we have to construct a year vector and provide it to toe predict functions.
(
lead.x0 <- data.frame(year = c(2014))
lead.estimatexO <- cbind(lead.x0,
    fit = predict(linModTrans, lead.x0),
    conf = predict(linModTrans, lead.x0, interval = "confidence"),
    pred = predict(linModTrans, lead.x0, interval = "predict")
    )
)
# We now have three versions of the fitted line! 
# Get rid of the extra ones by setting them to NULL:
lead.estimatex0$conf.fit <- lead.estimates$pred.fit <- NULL
(lead.estimates)

#### PLOT ####

ggplot(data = lead.estimates, aes())


