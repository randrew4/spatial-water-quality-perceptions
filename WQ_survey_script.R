#load package synchrony
WQsurvey<- read.csv("C:\\randrew4\\Desktop\\AFI\\Research Documents\\vario1.csv", header=T, sep=",")
income<- read.csv("C:\\randrew4\\Desktop\\AFI\\Research Documents\\vario_income.csv", header=T, sep=",")
education<- read.csv("C:\\randrew4\\Desktop\\AFI\\Research Documents\\vario_education.csv", header=T, sep=",")

wqQ11g <-read.csv("C:\\randrew4\\Desktop\\AFI\\Research Documents\\varioQ11g.csv", header=T, sep=",")
wqQ12e <-read.csv("C:\\randrew4\\Desktop\\AFI\\Research Documents\\varioQ12e.csv", header=T, sep=",")

head(WQsurvey)  
head(wqQ11g)

attach(WQsurvey)


library(synchrony)

meancorr (bktcorr, nrands = 999, alternative = "two.tailed",
          method = "pearson", type = 2)

semiv<-vario(data=WQsurvey, extent=1)

var.gaussian <- vario.fit (semiv$vario, semiv$mean.bin.dist, type = "gaussian")

var.spherical <- vario.fit (semiv$vario, semiv$mean.bin.dist, type = "spherical")

var.linear <- vario.fit (semiv$vario,semiv$mean.bin.dist, type = "linear")

sync.WQ <- vario (n.bins = 10, data = WQsurvey,
                   type = "semivar", extent = 1, nrands = 999,
                   is.centered = TRUE, alternative = "two", quiet = TRUE)


plot(sync.WQ$mean.bin.dist, sync.WQ$vario, xlab="Lag distance (km)", ylab="Semivariance")

##### for overall water quality perception
vario1<-vario (n.bins = 20, size.bins = 10, extent = 1, WQsurvey, data2 = NULL,
               is.latlon = TRUE, is.centered = FALSE, nrands = 999,
               type = "semivar",
               alternative = "one.tailed",
               mult.test.corr = "none",
               regional = "all",
               quiet = FALSE)

vario.fit1 (vario1, bins, weights = rep(1, length(vario)),
           type = c("spherical", "gaussian", "nugget", "linear",
                    "exponential", "sill", "periodic", "hole"),
           start.vals = list(c0 = 0, c1 = max(vario),
                             a = max(bins)/4, b=0.1, c=0.1),
           control = list(maxit=10000))


mod.sph<-vario.fit(vario1$vario, vario1$mean.bin.dist, type="spherical")
mod.exp<-vario.fit(vario1$vario, vario1$mean.bin.dist, type="exponential")
mod.gau<-vario.fit(vario1$vario, vario1$mean.bin.dist, type="gaussian")
mod.lin<-vario.fit(vario1$vario, vario1$mean.bin.dist, type="linear")


plot(vario1$mean.bin.dist, vario1$vario, xlab="Lag distance (km)", ylab="Semivariance")


lines(vario1$mean.bin.dist, mod.sph$fit, col="red")
lines(vario1$mean.bin.dist, mod.exp$fit, col="black")
lines(vario1$mean.bin.dist, mod.gau$fit, col="blue")
lines(vario1$mean.bin.dist, mod.lin$fit, col="green")
legend(x="topleft", legend=paste(c("Spherical AIC:", "Exponential AIC:","Gaussian AIC:", "Linear AIC:"),
                                 c(format(mod.sph$AIC, dig=3),
                                   format(mod.exp$AIC, dig=3),
                                   format(mod.gau$AIC, dig=3),
                                   format(mod.lin$AIC, dig=3))), lty=1, col=c("red", "black", "blue", "green"),
       bty="n")


mod.exp
vario1

#######################################################################################
### for income
vario2<-vario (n.bins = 20, size.bins = 10, extent = 1, income, data2 = NULL,
               is.latlon = TRUE, is.centered = FALSE, nrands = 999,
               type = "semivar",
               alternative = "one.tailed",
               mult.test.corr = "none",
               regional = "all",
               quiet = FALSE)

mod.sph2<-vario.fit(vario2$vario, vario2$mean.bin.dist, type="spherical")
mod.exp2<-vario.fit(vario2$vario, vario2$mean.bin.dist, type="exponential")
mod.gau2<-vario.fit(vario2$vario, vario2$mean.bin.dist, type="gaussian")
mod.lin2<-vario.fit(vario2$vario, vario2$mean.bin.dist, type="linear")


plot(vario2$mean.bin.dist, vario2$vario, xlab="Lag distance (km)", ylab="Semivariance")


lines(vario2$mean.bin.dist, mod.sph2$fit, col="red")
lines(vario2$mean.bin.dist, mod.exp2$fit, col="black")
lines(vario2$mean.bin.dist, mod.gau2$fit, col="blue")
lines(vario2$mean.bin.dist, mod.lin2$fit, col="green")
legend(x="topleft", legend=paste(c("Spherical AIC:", "Exponential AIC:","Gaussian AIC:", "Linear AIC:"),
                                 c(format(mod.sph2$AIC, dig=3),
                                   format(mod.exp2$AIC, dig=3),
                                   format(mod.gau2$AIC, dig=3),
                                   format(mod.lin2$AIC, dig=3))), lty=1, col=c("red", "black", "blue", "green"),
       bty="n")


mod.exp2
vario2

#######################################################################################
### for education
vario3<-vario (n.bins = 20, size.bins = 10, extent = 1, education, data2 = NULL,
               is.latlon = TRUE, is.centered = FALSE, nrands = 999,
               type = "semivar",
               alternative = "one.tailed",
               mult.test.corr = "none",
               regional = "all",
               quiet = FALSE)

mod.sph3<-vario.fit(vario3$vario, vario3$mean.bin.dist, type="spherical")
mod.exp3<-vario.fit(vario3$vario, vario3$mean.bin.dist, type="exponential")
mod.gau3<-vario.fit(vario3$vario, vario3$mean.bin.dist, type="gaussian")
mod.lin3<-vario.fit(vario3$vario, vario3$mean.bin.dist, type="linear")


plot(vario3$mean.bin.dist, vario3$vario, xlab="Lag distance (km)", ylab="Semivariance")


lines(vario3$mean.bin.dist, mod.sph3$fit, col="red")
lines(vario3$mean.bin.dist, mod.exp3$fit, col="black")
lines(vario3$mean.bin.dist, mod.gau3$fit, col="blue")
lines(vario3$mean.bin.dist, mod.lin3$fit, col="green")
legend(x="topleft", legend=paste(c("Spherical AIC:", "Exponential AIC:","Gaussian AIC:", "Linear AIC:"),
                                 c(format(mod.sph3$AIC, dig=3),
                                   format(mod.exp3$AIC, dig=3),
                                   format(mod.gau3$AIC, dig=3),
                                   format(mod.lin3$AIC, dig=3))), lty=1, col=c("red", "black", "blue", "green"),
       bty="n")


mod.sph3
vario3

########### spatial correllograms #########
library(ncf)
ncf.cor <- correlog(WQsurvey$long, WQsurvey$lat, WQsurvey$Q1_Overall,
                    increment=20, resamp=500)



##########################################################
### linear mixed models for WQ perceptions ###########
WQ<- read.csv("C:\\randrew4\\Desktop\\AFI\\Research Documents\\WQ_R.csv", header=T, sep=",")

head(WQ)


library(lme4)
attach(WQ)
County <- factor(County)
Status <- factor(Status)

plot(dist_to_spill, Q1_Overall)

#explore response variables

hist(WQ$Q1_Overall)


#independece across County/ARC status
boxplot(Q1_Overall ~ Status, data = WQ, notch = FALSE, ylab="Overall WQ Perception Score")  #looks like something could be going on here
boxplot(Q40_INCOME ~ Status, data = WQ, notch=TRUE, ylab="Self-Reported Income Level")  #looks like something could be going on here
boxplot(Q37_EDU ~ Status, data = WQ, notch=FALSE, ylab="Self-Reported Education Level")  #looks like something could be going on here

## ANOVAs of variables given county economic status
fit1<- aov(Q1_Overall ~ Status)
summary(fit1)
drop1(fit1,~.,test="F") # type III SS and F Tests
# Tukey Honestly Significant Differences
TukeyHSD(fit1) 

tapply(WQ$Q1_Overall, WQ$Status, summary)
library(gplots)

plotmeans(WQ$Q1_Overall~WQ$Status,xlab="ARC County Status",
          ylab="Overall Water Quality Perception Score", main="", connect=FALSE)

fit2<- aov(Q40_INCOME ~ Status)
summary(fit2)
# Tukey Honestly Significant Differences
TukeyHSD(fit2) 
tapply(WQ$Q40_INCOME, WQ$Status, summary)
plotmeans(WQ$Q40_INCOME~WQ$Status,xlab="ARC County Status",
          ylab="Self-Reported Income Score", main="", connect=FALSE)


fit3<- aov(Q37_EDU ~ Status)
summary(fit3)
# Tukey Honestly Significant Differences
TukeyHSD(fit3) 
tapply(WQ$Q37_EDU, WQ$Status, summary)
plotmeans(WQ$Q37_EDU~WQ$Status,xlab="ARC County Status",
          ylab="Self-Reported Education Score", main="", connect=FALSE)


#standarize and scale explantory variables of interest
WQ$elevationS <- scale(WQ$elevation)

WQ$dist_to_spillS <- scale(WQ$dist_to_spill)

#mixed effect model for County as random effect (grouping variable)
mixed.lmer <- lmer(Q1_Overall ~ elevationS + Q37_EDU + Q40_INCOME + dist_to_spillS + (1|County), data = WQ)
summary(mixed.lmer)
mixed.lmer

#plot to check for patterns that violate assumptions
plot(mixed.lmer)  # looks alright, no patterns evident
qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))  # points fall nicely onto the line - good!

#variance for County = 0.09009 and residual variance is 0.79553 so total variance is 0.88562 so 0.09009/0.88562 = 0.101759 ~10.2% variance explained by County after variance explained by fixed effect

#use package stargazer to create and output table
library(stargazer)
stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

##plot variable relationship patterns
library(ggplot2)
ggplot(WQ, aes(x = elevation, y = Q1_Overall)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Elevation (m)", y = "Overall Water Quality Perception Score")

ggplot(WQ, aes(x = Q40_INCOME, y = Q1_Overall)) +
  geom_point() +
  geom_smooth(method = "lm") 


#plot by color for stream
ggplot(WQ, aes(x = elevation, y = Q1_Overall, colour = County)) +
  geom_point(size = 2) +
  theme_classic() +
  theme(legend.position = "none")
