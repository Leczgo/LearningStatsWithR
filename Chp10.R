# Chapter Ten: Estimation <codecell> setup
library("lsr")
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
