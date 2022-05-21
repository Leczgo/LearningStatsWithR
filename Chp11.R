# Chapter Eleven: Hypothesis Testing <codecell> setup
library("lsr")
# <codecell> construct a binomial sampling distribution
ESP <- matrix(rbinom(n = 50000,size = 100,prob = 0.5),5,10000)
ESP <- colMeans(ESP)
hist(ESP)
# <codecell> running a binomial test
binom.test(x = 62,n = 100,p = 0.5)
