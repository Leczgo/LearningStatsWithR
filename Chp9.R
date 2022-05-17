# Chapter Nine: Probability Theory <codecell> setup
library("lsr")
# <codecell> the binomial distribution
dbinom(x = 4, size = 20,prob = 1/6)
pbinom(q = 4, size = 20, prob = 1/6)
qbinom( p = 0.75,size = 20,prob = 1/6)
rbinom(n = 3,size = 20,prob = 1/6)
# <codecell> the normal distribution
dnorm(x = 1,mean = 1,sd = 0.1)
# <codecell> other useful distributions
normal.a <- rnorm(n = 1000,mean = 0,sd = 1)
hist(normal.a)
normal.b <- rnorm(n = 1000,mean = 0,sd = 1)
normal.c <- rnorm(n = 1000,mean = 0,sd = 1)
# creating a chi-squared distributed variable from normally distrubted variables
chi.a <- (normal.a ^ 2) + (normal.b ^ 2) + (normal.c ^2)
hist(chi.a)
# creating a t-distrubited variable from normal and scaled chi-square variables
scaled.chi.3 <- chi.a / 3
normal.d <- rnorm(n = 1000,mean = 0,sd = 1)
t.3 <- normal.d / sqrt(scaled.chi.3)
hist(t.3)
# creating a F-distributed variable from two Chi-squared variables
chi.20 <- rchisq(n = 1000, df = 20)
scaled.chi.20 <- chi.20 / 20
f.3.20 <- scaled.chi.3 / scaled.chi.20
hist(f.3.20)
