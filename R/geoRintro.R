## ------------------------------------
## Commands in the geoRintro web-page
## ------------------------------------
##
## This is a script file illustrating some features of geoR 
## It may be used as an introduction to usage of geoR
##
## Only a few key commands and options are illustrated here.
## This is NOT an exaustive demonstration of the package resources.
##
## The commands are for illustrative purposes only.
## We do not attempt to perform a definitive analysis of the data.
##
##
## 1. Sourcing the package
##
## uncomment one of the following inf necessary
require(geoR)
##library(geoR, lib.loc="PATH_TO_geoR")

data(s100)
par.ori <- par(no.readonly=TRUE)

##
## 2. Descriptive plots
##
##jpeg("s100plot/s100p01.jpeg", wid=450, hei=450)
plot(s100)
##dev.off()
##
##jpeg("s100plot/s100p02.jpeg", wid=450, hei=450)
par(mfrow = c(2,2), mar=c(3,3,1,1), mgp=c(2,0.8,0))
points(s100, xlab = "Coord X", ylab = "Coord Y")
points(s100, xlab = "Coord X", ylab = "Coord Y", pt.divide = "rank.prop")
points(s100, xlab = "Coord X", ylab = "Coord Y", cex.max = 1.7, col = gray(seq(1, 0.1, l=100)), pt.divide = "equal")
points(s100, pt.divide = "quintile", xlab = "Coord X", ylab = "Coord Y")
##dev.off()
par(par.ori)

##
## 3. Variograms
##
cloud1 <- variog(s100, option = "cloud", max.dist=1)
cloud2 <- variog(s100, option = "cloud", estimator.type = "modulus", max.dist=1)
bin1 <- variog(s100, uvec=seq(0,1,l=11))
bin2 <- variog(s100, uvec=seq(0,1,l=11), estimator.type= "modulus")

##jpeg("s100plot/s100p03.jpeg", wid=450, hei=500)
par(mfrow=c(2,2),mar=c(3,3,2,2),mgp=c(2,0.8,0))
plot(cloud1, main = "classical estimator")
plot(cloud2, main = "modulus estimator")
plot(bin1, main = "classical estimator")
plot(bin2, main = "modulus estimator")
##dev.off()
par(par.ori)

##
bin1 <- variog(s100, uvec = seq(0,1,l=11), bin.cloud = T)
bin2 <- variog(s100, uvec = seq(0,1,l=11), estimator.type = "modulus", bin.cloud = T)

##jpeg("s100plot/s100p04.jpeg", wid=450, hei=250)
par(mfrow=c(1,2),mar=c(3,3,2,2),mgp=c(2,.8,0))
plot(bin1, bin.cloud=T, main="classical estimator")
plot(bin2, bin.cloud=T, main="modulus estimator")
##dev.off()
par(par.ori)

##
bin1 <- variog(s100, uvec = seq(0,1,l=11))

##jpeg("s100plot/s100p05.jpeg", wid=250, hei=250)
par(mar=c(3,3,.2,.2),mgp=c(2,.8,0))
plot(bin1)
lines.variomodel(cov.model="exp", cov.pars=c(1,0.3), nugget = 0, max.dist = 1, lwd = 3)
smooth <- variog(s100, option = "smooth", max.dist = 1, n.points = 100, kernel = "normal", band = 0.2)
lines(smooth, type ="l", lty = 2)
legend(0.4, 0.3, c("empirical","exponential model","smooth"), lty=c(1,1,2), lwd=c(1,3,1), cex=0.7)
##dev.off()
par(par.ori)

##
vario60 <- variog(s100, max.dist = 1, direction=pi/3) 
vario.4 <- variog4(s100, max.dist = 1)

##jpeg("s100plot/s100p05a.jpeg", wid=500, hei=275)
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,.8,0))
plot(vario60)
title(main = expression(paste("directional, angle = ", 60 * degree)))
plot(vario.4)
##dev.off()
par(par.ori)

##
## 4. Parameter estimation
##
##
## Fitting by eye
##
##jpeg("s100plot/s100p05b.jpeg", wid=250, hei=250)
par(mar=c(3.5,3.5,.2,.2),mgp=c(2,.8,0))
plot(variog(s100, max.dist=1))
lines.variomodel(cov.model="exp", cov.pars=c(1,.3), nug=0, max.dist=1)
lines.variomodel(cov.model="mat", cov.pars=c(.85,.2), nug=0.1, kappa=1,max.dist=1, lty=2)
lines.variomodel(cov.model="sph", cov.pars=c(.8,.8), nug=0.1,max.dist=1, lwd=2)
##dev.off()
##
## Fitting models with nugget fixed to zero
##
ml <- likfit(s100, ini = c(1,0.5), fix.nugget = T)
reml <- likfit(s100, ini = c(1,0.5), fix.nugget = T, method = "RML")
ols <- variofit(bin1, ini = c(1,0.5), fix.nugget = T, weights="equal")
wls <- variofit(bin1, ini = c(1,0.5), fix.nugget = T)
##
## Fitting models with a fixed value for the nugget
##
ml.fn <- likfit(s100, ini = c(1,0.5), fix.nugget = T, nugget = 0.15)
reml.fn <- likfit(s100, ini = c(1,0.5), fix.nugget = T, nugget = 0.15, method = "RML")
ols.fn <- variofit(bin1,ini = c(1,0.5), fix.nugget = T, nugget = 0.15, weights="equal")
wls.fn <- variofit(bin1, ini = c(1,0.5), fix.nugget = T, nugget = 0.15)
##
## Fitting models estimated nugget<br>
##
ml.n <- likfit(s100, ini = c(1,0.5), nug = 0.5)
reml.n <- likfit(s100, ini = c(1,0.5), nug = 0.5, method = "RML")
ols.n <- variofit(bin1, ini = c(1,0.5), nugget=0.5, weights="equal")
wls.n <- variofit(bin1, ini = c(1,0.5), nugget=0.5)
##
save.image()

##jpeg("s100plot/s100p06.jpeg", wid=650, hei=300)
par(mfrow=c(1,3), mar=c(3,3,2,0.5), mgp=c(2,.8,0), cex=1)
plot(bin1, main = expression(paste("fixed ", tau^2 == 0)))
lines(ml, max.dist = 1)
lines(reml, lwd = 2, max.dist = 1)
lines(ols, lty = 2, max.dist = 1)
lines(wls, lty = 2, lwd = 2, max.dist = 1)
legend(0.55, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1,1,2,2), lwd = c(1,2,1,2), cex=0.7)
##
plot(bin1, main = expression(paste("fixed ", tau^2 == 0.15)))
lines(ml.fn, max.dist = 1)
lines(reml.fn, lwd = 2, max.dist = 1)
lines(ols.fn, lty = 2, max.dist = 1)
lines(wls.fn, lty = 2, lwd = 2, max.dist = 1)
legend(0.55, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1,1,2,2), lwd = c(1,2,1,2), cex=0.7)
##
plot(bin1, main = expression(paste("estimated  ", tau^2)))
lines(ml.n, max.dist = 1)
lines(reml.n, lwd = 2, max.dist = 1)
lines(ols.n, lty = 2, max.dist = 1)
lines(wls.n, lty =2, lwd = 2, max.dist = 1)
legend(0.55, 0.3, legend = c("ML", "REML", "OLS", "WLS"), lty = c(1,1,2,2), lwd = c(1,2,1,2), cex=0.7)
##dev.off()
par(par.ori)
##

ml.n  
summary(ml.n)

##
## 5. Variogram Envelopes
##
env.mc <- variog.mc.env(s100, obj.var=bin1)
env.model <- variog.model.env(s100, obj.var=bin1, model=wls)

##jpeg("s100plot/s100p07.jpeg", wid=500, hei=250)
par(mfrow=c(1,2), mar=c(3,3,0.2,0.2), mgp=c(2,.8,0))
plot(bin1, envelope=env.mc)
plot(bin1, envelope=env.model)
par(par.ori)
##dev.off()
##
## 6. Profile likelihood
##
prof <- proflik(ml, geodata = s100, sill.val = seq(0.48, 2, l=11), range.val = seq(0.1, 0.52, l=11), uni.only = FALSE)
save.image()

##jpeg("s100plot/s100p08.jpeg", wid=650, hei=300)
par(mfrow=c(1,3), mar=c(3,3,1,0.2), mgp=c(2,.8,0), cex=1.2)
plot(prof, nlevels=16)
##dev.off()
par(par.ori)

##
## 7. Cross-validation
##
xv.ml <- xvalid(s100, model=ml)
xv.wls <- xvalid(s100, model=wls)
xvR.ml <- xvalid(s100, model=ml, reest=TRUE)
xvR.wls <- xvalid(s100, model=wls, reest=TRUE, variog.obj=bin1)
save.image()
##

##jpeg("s100plot/s100p08a.jpeg", wid=500, hei=800)
par(mfcol = c(5,2), mar=c(2.5,2.5,.5,.5), mgp=c(1.8,0.8,0))
plot(xv.wls)
##dev.off()
par(par.ori)
##
## 8. Kriging 
##
##jpeg("s100plot/s100p09.jpeg", wid=300, hei=300)
par(mar=c(3,3,0.2,0.2), mgp=c(2,.8,0))
plot(s100$coords, xlim=c(0,1.2), ylim=c(0,1.2), xlab="Coord X", ylab="Coord Y")
loci <- matrix(c(0.2, 0.6, 0.2, 1.1, 0.2, 0.3, 1.0, 1.1), ncol=2)
text(loci, as.character(1:4), cex=1.3, col="red")
polygon(x=c(0,1,1,0), y=c(0,0,1,1), lty=2)
##dev.off()
par(par.ori)


kc4 <- krige.conv(s100, locations = loci, krige = krige.control(obj.m = wls))
## defining the grid
pred.grid <-  expand.grid(seq(0,1, l=51), seq(0,1, l=51))
#pred.grid <-  expand.grid(seq(0,1, l=101), seq(0,1, l=101))
## kriging calculations
kc <- krige.conv(s100, loc = pred.grid, krige = krige.control(obj.m = ml))
save.image()

## displaying predicted values
##jpeg("s100plot/s100p10.jpeg", wid=350, hei=350)
par(mar=c(3,3,.5,.5), mgp=c(2,.8,0))
image(kc, loc = pred.grid, col=gray(seq(1,0.1,l=30)), xlab="Coord X", ylab="Coord Y")
##dev.off()
par(par.ori)

##
## 9. Bayesian prediction
##
pr <- prior.control(phi.discrete = seq(0, 5, l=101), phi.prior="rec")
bsp4 <- krige.bayes(s100, loc = loci, prior = pr, output = output.control(n.post=10000))
save.image()

##jpeg("s100plot/s100p11.jpeg", wid=650, hei=300)
par(mfrow=c(1,3), mar=c(3,3,1,0.2), mgp=c(2,.8,0), cex=1.2)
hist(bsp4$posterior$sample$beta, main="", xlab=expression(beta), prob = T)
hist(bsp4$posterior$sample$sigmasq, main="", xlab=expression(sigma^2), prob = T)
hist(bsp4$posterior$sample$phi, main="", xlab=expression(phi), prob = T)
##dev.off()
par(par.ori)

##jpeg("s100plot/s100p12.jpeg", wid=300, hei=300)
par(mar=c(3,3,.5,.5),mgp=c(2,.8,0))
plot(bin1, ylim = c(0,1.5))
lines(bsp4, max.dist = 1.2, summ = mean)
lines(bsp4, max.dist = 1.2, summ = median, lty = 2)
lines(bsp4, max.dist = 1.2, summ = "mode", post="par",lwd = 2, lty = 2)
legend(0.25, 0.4, legend = c("variogram posterior mean", "variogram posterior median", "parameters posterior mode"), lty = c(1,2,2), lwd = c(1,1,2), cex = 0.8)
##dev.off()
par(par.ori)
##

##jpeg("s100plot/s100p13.jpeg", wid=460, hei=500)
par(mfrow=c(2,2), mar=c(3,3,.5,.5), mgp=c(1.5,.8,0))
for(i in 1:4){
  ## curve(dnorm(x, mean=kc4$pred[i], sd=sqrt(kc4$krige.var[i])), from=kc4$pred[i] - 3*sqrt(kc4$krige.var[i]), kc4$pred[i] +3*sqrt(kc4$krige.var[i]))
  kpx <- seq(kc4$pred[i] - 3*sqrt(kc4$krige.var[i]), kc4$pred[i] +3*sqrt(kc4$krige.var[i]), l=100)
  kpy <- dnorm(kpx, mean=kc4$pred[i], sd=sqrt(kc4$krige.var[i]))
  bp <- density(bsp4$predic$sim[i,])
  rx <- range(c(kpx, bp$x))
  ry <- range(c(kpy, bp$y))
  plot(cbind(rx, ry), type="n", xlab=paste("Location", i), ylab="density", xlim=c(-4, 4), ylim=c(0,1.1))
  lines(kpx, kpy, lty=2)
  lines(bp)}
##dev.off()
par(par.ori)

## definig grid
pred.grid <-  expand.grid(seq(0,1, l=51), seq(0,1, l=51))
#pred.grid <-  expand.grid(seq(0,1, l=101), seq(0,1, l=101))
## Bayesian prediction
bsp <- krige.bayes(s100, loc = pred.grid, prior = prior.control(phi.discrete = seq(0,5,l=101)), output=output.control(n.predictive=2))
save.image()

##jpeg("s100plot/s100p14.jpeg", wid=600, hei=600)
par(mfrow=c(2,2), mar=c(2.5,2.5,3,0), mgp=c(1.5,.8, 0))
image(bsp, loc = pred.grid, main = "predicted", col=gray(seq(1,0.1,l=30)))
image(bsp, val ="variance", loc = pred.grid, main = "prediction variance", col=gray(seq(1,0.1,l=30)))
image(bsp, val = "simulation", number.col = 1, loc = pred.grid, main = "a simulation from\nthe predictive distribution", col=gray(seq(1,0.1,l=30)))
image(bsp, val = "simulation", number.col = 2,loc = pred.grid, main = "another simulation from \n the predictive distribution", col=gray(seq(1,0.1,l=30)))
##dev.off()
par(par.ori)

##
## 10. Simulation
##
sim1 <- grf(100, cov.pars=c(1, .25))

##jpeg("s100plot/s100p15.jpeg", wid=500, hei=250)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.2),mgp=c(1.5,.8,0))
points.geodata(sim1, main="simulated data") 
plot(sim1, max.dist=1, main="true and empirical variograms")
##dev.off()

sim2 <- grf(441, grid="reg", cov.pars=c(1, .25))

##jpeg("s100plot/s100p16.jpeg", wid=350, hei=350)
par(mar=c(2,2,1,0.2),mgp=c(1.5,.8,0))
image(sim2, main="a \"smallish\" simulation", col=gray(seq(1, .1, l=30)))
##dev.off()

sim3 <- grf(40401, grid="reg", cov.pars=c(10, .2), met="circ") 

##jpeg("s100plot/s100p17.jpeg", wid=350, hei=350)
par(mar=c(2,2,1,0.2),mgp=c(1.5,.8,0))
image(sim3, main="simulation in a finner grid", col=gray(seq(1, .1, l=30)))
##dev.off()
##
