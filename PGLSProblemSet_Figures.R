
library(ape)
library(picante)
library(caper)

primatedata <- read.table("Primatedata.txt", sep = "\t", header = TRUE)
primatetree <- read.nexus("consensusTree_10kTrees_Version2.nex") 

plot(primatetree)

plot(primatetree, cex = 0.5)

zoom(primatetree, list(grep("Cercopithecus", primatetree$tip.label)), subtree = FALSE)

zoom(primatetree, list(grep("Cercopithecus", primatetree$tip.label)), subtree = TRUE)

par(mfrow = c(1,1)) 
plot(primatetree, type = "fan", edge.color = "deeppink", tip.color = "green", cex = 0.5)

plot(primatetree)
axisPhylo()

primatetree <- multi2di(primatetree) 

primatetreereroot <- root(primatetree, "Saimiri_sciureus") #note I just #chose this species at random!
plot(primatetreereroot)

row.names(primatedata) <- primatedata$Binomial

par(mfrow = c(2, 2))

hist(primatedata$AdultBodyMass_g, col = rainbow(8))
hist(primatedata$GestationLen_d, col = rainbow(8))
hist(log(primatedata$AdultBodyMass_g), col = rainbow(8))
hist(log(primatedata$GestationLen_d), col = rainbow(8))

model.ols <- lm(log(GestationLen_d) ~ log(AdultBodyMass_g), data = primatedata)
plot(log(GestationLen_d) ~ log(AdultBodyMass_g), data = primatedata)
abline(model.ols)
points(log(GestationLen_d[Family == "Cercopithecidae"]) ~ log(AdultBodyMass_g[Family == "Cercopithecidae"]), 
data = primatedata, col= "blue",  pch = 16)

primate <- comparative.data(phy = primatetree, data = primatedata, names.col = Binomial, vcv = TRUE, na.omit = FALSE, warn.dropped = TRUE)

model.pgls <- pgls(log(GestationLen_d) ~ log(AdultBodyMass_g), data = primate, lambda='ML')
plot(log(GestationLen_d) ~ log(AdultBodyMass_g), data = primate$data)
abline(model.pgls)

lambda.profile <- pgls.profile(model.pgls, 'lambda')
plot(lambda.profile)

par(mfrow = c(2,2))
plot(model.pgls)