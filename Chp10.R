# Chapter Ten: Estimation <codecell> setup
library("lsr")
library("sciplot")
load("data/afl24.Rdata")
# <codecell> the law of large numbers
IQ <- rnorm(n = 10000,mean = 100, sd = 15)
IQ <- round( IQ )
# <codecell> sampling distributions
IQ.1 <- round( rnorm(n = 5, mean = 100, sd = 15) )
hist(IQ)
IQmean <- c(1:10000)
for (i in IQmean) {
  norm <- mean(rnorm(n = 5,mean = 100,sd = 15))
  IQmean[i] <- IQmean[i] * 0 + norm
}
hist(IQmean,freq = FALSE)
curve(dnorm(x,mean = 100,sd = 15),add = TRUE)
# <codecell> Estimating Population Parameters
# simulating sampling distributions of the standard deviation
sd.2 <- matrix(rnorm(n = 100000,mean = 100,sd = 15),2,50000)
sd.2 <- apply(sd.2,2,FUN = sd)
hist(sd.2)
abline(v = c(mean(sd.2),15))
# <codecell> simulating the sample mean and sample standard deviations at different sample sizes
ss <- c(1:10)
mn <- ss
std <- ss
n <- data.frame(ss,mn,std)
for (i in c(1:10)) {
  norm <- matrix(rnorm(n = i * 10000,mean = 100,sd = 15),i,10000)
  norm.mean <- colMeans(norm)
  norm.sd <- apply(norm,2,FUN = sd)
  n$mn[i] <- mean(norm.mean)
  n$std[i] <- mean(norm.sd)
}
plot(x = n$ss,y = n$mn)
plot(x = n$ss,y = n$std)
# <codecell> estimating a confidence interval
ciMean(x = afl$attendance)
# plotting confidence intervals
bargraph.CI(x.factor = year,    # found in sciplot package
  response = attendance,
  data = afl,
  ci.fun = ciMean,
  xlab = "Year",
  ylab = "Average Attendance")
lineplot.CI(    # found in sciplot package
  x.factor = year,
  response = attendance,
  data = afl,
  xlab = "Year",
  ylab = "Average Attendance"
)
