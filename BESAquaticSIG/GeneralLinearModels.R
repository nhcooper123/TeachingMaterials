lines(fit ~ X, data = , subset = SEASON == "summer", col =1)

# Clear R's brain
rm(list=ls())

#----------------------------------------------------
# general linear models - regression
#----------------------------------------------------

# Load the data and look at it
plant.growth <- read.csv("plant.growth.rate.csv")
str(plant.growth)

# PLOT YOUR DATA
par(mfrow=c(1,1))
plot(plant.growth.rate ~ soil.moisture.content, data = plant.growth,
	xlab = "Soil moisture content", xlim = c(0,2),
	ylab = "Plant growth rate",
	cex = 2, pch = 21, bg = "cornflowerblue")

# make a model
model1 <- lm(plant.growth.rate ~ soil.moisture.content, data = plant.growth)

## Check the diagnostics
par(mfrow = c(2, 2))
plot(model1)
par(mfrow = c(1, 1))

## Check the diagnostics
par(mfrow = c(2, 3))
plot(model1)
hist(model1$resid, col = "grey")
par(mfrow = c(1, 1))

# Evaluate the model
anova(model1) # produces the Sums of squares anova table
summary(model1) # produces table of coefficients

# plot the fit on the data - SIMPLE METHOD
plot(plant.growth.rate ~ soil.moisture.content, data = plant.growth,
     xlab = "Soil moisture content", 
     ylab = "Plant growth rate",
     cex = 2, pch = 21, bg = "cornflowerblue")

abline(model1, col = "red")

# ADDING fitted values to your figure

# make new X
newX <- expand.grid(soil.moisture.content = 
	seq(from = 0.2, to = 2, length = 20))

# make new Y's (predictions)
newY <- predict(model1, newdata = newX, interval = "confidence")

# housekeeping
addThese <- data.frame(newX, newY)
head(addThese)

# Add the lines - 3 of them - FROM THE addThese data frame
lines(fit ~ soil.moisture.content, data = addThese)
lines(lwr ~ soil.moisture.content, data = addThese, 
	lty = 2, col = 'deeppink')
lines(upr ~ soil.moisture.content, data = addThese,
	lty = 2, col = 'deeppink')

#----------------------------------------------------
# general linear models - ANOVA - daphniagrowth.csv
#----------------------------------------------------
rm(list=ls())

## read in the data
daphnia <- read.csv("Daphniagrowth.csv")

## look at the structure of the data
str(daphnia)

## a quick exploratory plot

plot(growth.rate ~ parasite, data = daphnia)

boxplot(growth.rate ~ parasite, data = daphnia)

# to set a level, before the model
# dd$parasite <- relevel(dd$parasite, ref = "whatever")

# BUILD THE MODEL
model2 <- lm(growth.rate ~ parasite, data = daphnia)

## model diagnostics
par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

## model interpretation
anova(model2)
## the parasite treatment explains a large amount of variation in daphnia growth rate

summary(model2)
## Shows the effects of the parasites on growth rate, relative to the control (Intercept)

## --------------------- EXTRAS ----------------------------------------------

# How to fit a Tukey or other post-hoc comparison using the multcomp library
# install.packages("multcomp")
library(multcomp)

tukey.par <- glht(model2, linfct = mcp(parasite = "Tukey"))

summary(tukey.par)
plot(tukey.par)

## Set up orthogonal (planned) contrasts: rather than ALL pairwise....
## some specific examples
c1 <- c(-1, 1/3, 1/3, 1/3) # control vs overall parasite mean
c2 <- c(0, -1, 1/2, 1/2)   # parasite 1 vs 2 & 3
c3 <- c(0, 0, -1/2, 1/2)   # parasite 2 vs 3

# combine the hypotheses
mat <- cbind(c1, c2, c3)

# allocate them to the treatment
contrasts(dd$parasite) <- mat

# run the model returning THESE hypotheses
mmcheck <- lm(growth.rate ~ parasite, data = daphnia)
summary(mmcheck)

# read and plot data 
plot(growth.rate ~ parasite, data = daphnia)
abline(h = mean(subset(daphnia, parasite != "control")$growth.rate), lty = 2)

# fit the basic model and check termwise effect
mod1 <- lm(growth.rate ~ parasite, data = daphnia)
summary(mod1)

# now set up a contrast to compare copntrol to mean of all 3 parasites
levels(daphnia$parasite)
contrastmatrix <- cbind(c(0, 1, 1, 1))
contrasts(daphnia$parasite, how.many = 1) <- contrastmatrix

# rerun
mod2 <- lm(growth.rate ~ parasite, data = daphnia)
summary(mod2)


#----------------------------------------------------
# general linear models - ANCOVA
#----------------------------------------------------
rm(list=ls())

# Get the data
limpet <- read.csv("limpet.csv")

plot(EGGS ~ DENSITY, data = limpet, col = SEASON,
	xlab="DENSITY", ylab="EGGS", pch = 19, cex = 2)

legend("topright", c("spring","summer"), pch = 19, cex = 2, 
	col = c("black", "red"), bty = "n")

# make the model
model3 <- lm(EGGS ~ DENSITY * SEASON, data = limpet) # linear model with interaction

# check the assumptions
par(mfrow = c(2, 3))
plot(model3)
hist(resid(model3))
par(mfrow = c(1, 1))

# examine the results
summary(model3) # coefficients
anova(model3) # ANOVA tables with sequential sums of squares

#----------------------------------------------------#----------------------------------------------------
# construct a data frame of where you want predicted values 
# to be "located" on your graph
# play around with this to see what happens when you 
# fix DENSITY to its mean, etc....
#----------------------------------------------------#----------------------------------------------------
newX <- expand.grid(
	DENSITY = seq(from = 0, to = 60, length = 10),
	SEASON = levels(limpet$SEASON))
newX

# NOW combine in a data.frame newX and predicted values generated
# by predict, using the newdata argument you can add se.fit=TRUE to this if you want

newY <- predict(model3, newdata = newX, interval = "confidence")
newY

# Houskeeping
addThese <- data.frame(newX, newY)
addThese

# remake the original plot
plot(EGGS ~ DENSITY, data = limpet, col = SEASON,
	xlab="DENSITY", ylab="EGGS", pch = 19, cex = 2)

legend("topright", c("spring","summer"), pch = 19, cex = 2, 
	col = c("black", "red"), bty = "n")

# NOW add the lines/points you made - each one of these 
# grabs a subset of the PlotThis data corresponding to SEASON 

# Summer
lines(fit ~ DENSITY, 
	data = subset(addThese, SEASON == "summer"), 
	col = "red")

lines(lwr ~ DENSITY,
	data = subset(addThese, SEASON == "summer"),
	col = "red", lty = 2)

lines(upr ~ DENSITY,
	data = subset(addThese, SEASON == "summer"),
	col = "red", lty = 2)

# Spring
lines(fit ~ DENSITY, 
	data = subset(addThese, SEASON == "spring"),
	col = "black")

lines(lwr ~ DENSITY,
	data = subset(addThese, SEASON == "spring"),
	col = "black", lty = 2)

lines(upr ~ DENSITY,
	data = subset(addThese, SEASON == "spring"),
	col = "black", lty = 2)

# Making transparent confidence bands in base graphics
# note refernce to lengths in addThese
xx <- c(seq(0, 60, length = 10), seq(60, 0, length = 10))
yy <- c(addThese$upr[1:10], rev(addThese$lwr[1:10]))
yy2 <- c(addThese$upr[11:20], rev(addThese$lwr[11:20]))

polygon(xx, yy, col = rgb(0.19,0.19,0.19,0.3), border=FALSE)
polygon(xx, yy2, col = rgb(0.19,0.19,0.19,0.3), border=FALSE)

## plot with ggplot2
# install.pacakges("ggplot2")
library(ggplot2)

## set up the plot
p <- ggplot(limpet, aes(x = DENSITY, y = EGGS, fill = SEASON))
## make the layers
l1 <- geom_point(size = 6, pch = 21) 
l2 <- geom_smooth(data = addThese, aes(ymin = lwr, ymax = upr, fill = SEASON, y = fit),
	stat = "identity", colour = "black")

## make the plot
p + l2 + l1

# Note that for glm there is no nice "confidence" option for confidence intervals in
# predict.glm so need to work these out yourself using fit + 1.96*se.fit instead of upr
# and fit - 1.96*se.fit for lwr.

#----------------------------------------------------
# general linear models - two way ANOVA - growth.csv
#----------------------------------------------------
rm(list=ls())

dd <- read.csv("growth.csv")

## an exploratory plot
stripplot(supplement ~ gain, groups=diet, dd, jitter=T, pch=19, cex=2, auto.key=T)
## or
with(dd, interaction.plot(supplement, diet, gain))

## make the supplement variable have the reference levels as the control treatment:
dd$supplement <- relevel(dd$supplement, ref="control")

## now the plot will have control at the bottom
stripplot(supplement ~ gain, groups=diet, dd, jitter=T, pch=19, cex=2, auto.key=T)
##
with(dd, interaction.plot(supplement, diet, gain))

## and the model will have the control treatment as the intercept.
## Note that we use * to fit the main effects and the interaction term
mm <- lm(gain ~ diet*supplement, dd)

## model diagnostics
par(mfrow=c(2, 2))
plot(mm)

## There is little evidence of an interaction, but strong main effects
anova(mm)

## Again, no clear interactions terms (there should not be,
## given the lack of a significant interaction term by F-test) 
summary(mm)
## The table looks rather scary, but is explained in the powerpoint presentation.

