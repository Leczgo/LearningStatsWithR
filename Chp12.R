# Chapter Twelve: Categorical Data Analysis & Chi-Squared Tests
# <codecell> setup
library("lsr")
load("data/randomness.Rdata")
load("data/chapek9.Rdata")
load("data/salem.Rdata")
load("data/agpp.Rdata")
# <codecell> Toodness of Fit test for analyzing cards data
str(cards)
head(cards)
observed <- table(cards$choice_1) # get real frequencies
print( observed )
observed["diamonds"]
# define expected probabilities
probabilities <- c("clubs" = 0.25,"diamonds" = 0.25,"hearts" = 0.25,"spades" = 0.25)
print(probabilities)
# define test statistics
N <- 200    # sample size
expected <- probabilities * N    # expected frequencies
print( expected )
expected
delta <- observed - expected    # get delta values
print( delta )
delta_sqr <- delta ^ 2    # get square of delta values
print( delta_sqr )    # removes negatives and makes large deltas more apparent
delta_ratio <- delta_sqr / expected    # standardize square values
print( delta_ratio )
GOF <- sum( delta_ratio )    #<=== Defines Goodness of Fit (Chi Squared) Test statistic
print( GOF )    # when GOF is small, observed values are closer to expected values
p.value <- 1 - pchisq(q = GOF,df = 3)
p.value <= 0.05
goodnessOfFitTest( cards$choice_1 )    # test function in 'lsr' package
nullProbs <- c(clubs = 0.2,diamonds = 0.3,hearts = 0.3,spades = 0.2)
goodnessOfFitTest(x = cards$choice_1,p = nullProbs)
# <codecell> Chi-Squared Independence Test
str(chapek9)
print( summary(chapek9) )
chapekFrequencies <- xtabs(~ choice + species, data = chapek9)
print(chapekFrequencies)
associationTest(~ choice + species, data = chapek9)    # using function from lsr to complete independence test
# manually create test statistic
Rs <- rowSums(chapekFrequencies)
Cs <- colSums(chapekFrequencies)
N <- sum(chapekFrequencies)
expectedProbs <- Rs / N
expectedFrequencies <- rbind(Cs,Cs,Cs) * expectedProbs
indTest <- ((chapekFrequencies - expectedFrequencies) ^ 2) / expectedFrequencies
indTest <- sum(indTest)
Chi.stat <- 1 - pchisq(q = indTest,df = 2)
Chi.schore <- qchisq(p = 0.95,df = 2)
# <codecell> Effect size
effect <- cramersV(chapekFrequencies)    # calculate the Cramer's V effect size using a function from the 'lsr' package
print( effect )
dim(chapekFrequencies)
# manually computing the Cramer's V effect size
k <- min( dim(chapekFrequencies) )
manEffect <- sqrt( indTest / N * (k - 1) )
print( manEffect )
# <codecell> performng Chi Squared Tests in R
chisq.test(x = observed)
chisq.test(x = observed, p = c(0.2,0.3,0.3,0.2))    # specifying probabilities
chisq.test( chapekFrequencies )    # running test of indpendence by submitting frequency table
# <codecell> Fisher's T-Test
salem.tabs <- table(trial)
print( salem.tabs )
chisq.test( salem.tabs )    # will return warning due to small sample size
fisher.test( salem.tabs )
# <codecell> McNemar's Test
str( agpp )
head( agpp )
summary( agpp )    # each subject has two responses
# table gets setup differently than normal test table
right.table <- xtabs( ~ response_before + response_after,data = agpp)
print( right.table )
mcnemar.test( right.table )
# running mcnemar test of cards data
cardChoices <- xtabs( ~ choice_1 + choice_2,data = cards )
print( cardChoices )
chisq.test( cardChoices )
mcnemar.test( cardChoices )
