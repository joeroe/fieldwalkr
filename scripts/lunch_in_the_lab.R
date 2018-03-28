# lunch_in_the_lab.R
# Simulating surface survey. Presentation @ UCL IoA, Thurs 19th Feb 2016
# Joe Roe <joseph.roe.12@ucl.ac.ul>

library("spatstat")
library("akima")
library("fieldwalkr")

par(mfrow=c(1,2))

# Generate some point patterns to survey: sparsely distributed sites
sites.mu <- 3
sites.var <- 0.1
sites <- rLGCP(mu=sites.mu, var=sites.var)
plot(sites, main="sites")

# ...and a dense artefact scatter
finds.mu <- 8
finds.var <- 4
finds <- rLGCP(mu=finds.mu, var=finds.var)
plot(finds, main="finds")

# Grid the area in a complete sample
cgrid <- sample.all(Window(sites), grid.size=0.1)
plot(cgrid, main="sites", cols=c("white", "white"))
plot(cgrid, main="finds", cols=c("white", "white"))

# Run the survey & look at the results
sites.survey <- survey(surface=sites, sample=cgrid, detection.function=detect.simple)
finds.survey <- survey(surface=finds, sample=cgrid, detection.function=detect.simple)
plot(sites.survey)
plot(finds.survey)

# Generate point patterns from the survey results and compare them to the original point patterns
sites.survey.points <- as.ppp(sites.survey)
finds.survey.points <- as.ppp(finds.survey)
par(mfrow=c(2,2))
plot(sites)
plot(sites.survey.points)
plot(finds)
plot(finds.survey.points)

# Plot KDEs of the survey results alongside those of the original point patterns
par(mfrow=c(2,2))
plot(density(sites))
plot(density(sites.survey.points))
plot(density(finds))
plot(density(finds.survey.points))

# Sample transects on a finer grid
transects <- sample.transect(Window(sites), n=8, grid.size=0.05, orientation=0)
par(mfrow=c(1,2))
plot(transects, main="sites")
plot(transects, main="finds")

# And run another set of surveys
sites.survey2 <- survey(surface=sites, sample=transects, detection.function=detect.simple)
finds.survey2 <- survey(surface=finds, sample=transects, detection.function=detect.simple)
sites.survey2.points <- as.ppp(sites.survey2)
finds.survey2.points <- as.ppp(finds.survey2)
par(mfrow=c(2,2))
plot(sites)
plot(sites.survey2.points)
plot(finds)
plot(finds.survey2.points)

# Interpolate the results
sites.survey2.interp <- interp.quadratcount(sites.survey2)
finds.survey2.interp <- interp.quadratcount(finds.survey2)
sites.survey2.interp.points <- as.ppp(sites.survey2.interp)
finds.survey2.interp.points <- as.ppp(finds.survey2.interp)
par(mfrow=c(1,2))
plot(sites.survey2.interp.points)
plot(finds.survey2.interp.points)

# Compare the results to the original point patterns
par(mfrow=c(2,2))
plot(density(sites))
plot(density(sites.survey2.interp.points))
plot(density(finds))
plot(density(finds.survey2.interp.points))

# Experiment with different numbers of transects

# PARAMETER ANALYSIS
# Compare the fitted LGCP model parameters to the original ones
kppm(finds.survey.points, clusters="LGCP")

# Repeat the whole analysis multiple times for a series of different grid sizes
par(mfrow=c(1,2))
gs <- sapply(c(0.25, 0.1, 0.05, 0.01, 0.005, 0.001), function(s) {
  gr <- sample.all(Window(finds), s)
  sv <- survey(finds, gr, detect.simple)
  svp <- as.ppp(sv)
  mod <- kppm(svp, clusters="LGCP")
  rmse <- sqrt(((finds.mu-mod$mu)^2+(finds.var-mod$modelpar["sigma2"])^2)/2)
  return(rmse)
})
plot(1:6, gs, xlab="grid size", ylab="rmse", xaxt="n")
axis(1, at=1:6, labels=c(0.25, 0.1, 0.05, 0.01, 0.005, 0.001))

# Or we could investigate the number of transects in a transect sample
ts <- sapply(c(2, 3, 4, 5, 8, 10), function(n) {
  gr <- sample.transect(Window(finds), n, 0.05)
  sv <- survey(finds, gr, detect.simple)
  svp <- as.ppp(sv)
  mod <- kppm(svp, clusters="LGCP")
  rmse <- sqrt(((finds.mu-mod$mu)^2+(finds.var-mod$modelpar["sigma2"])^2)/2)
  return(rmse)
})
plot(c(2, 3, 4, 5, 8, 10), gs, xlab="transects", ylab="rmse")
