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
linearModelTransformAddative <- lm(Pb ~ year, data = Pb_Jamtland)
linearModelTransformAddative.summary <- summary(linearModelTransformAddative)
