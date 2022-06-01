# Chapter 13: Comparing Two Means <codecell> setup
library("lsr")
load("data/zeppo.Rdata")
load("data/harpo.Rdata")
load("data/Chico.Rdata")
load("data/awesome.Rdata")
library("psych")
# <codecell> z-tests
print( grades )
mean( grades )
sample_mean <- mean( grades )
print(sample_mean)
mu.null <- 67.5
sd.true <- 9.5
N <- length( grades )
sem.true <- sd.true / sqrt(N)    # compute standard error
print(sem.true)
z.score <- (sample_mean - mu.null) / sem.true
print( z.score )
upper.area <- pnorm(q = z.score,lower.tail = FALSE)
print( upper.area )    # caclulate threshold of the upper tail
lower.area <- pnorm(q = -z.score,lower.tail = TRUE)
print( lower.area )
p.score <- upper.area + lower.area
print( p.score )
# <codecell> one-sample t tests
se.est <- sd( grades )    # computing the estimate of the sd
t.score <- (sample_mean - mu.null) / (se.est / sqrt(N) )    # computing the t test statistic
p.score <- 2 * (1 - pt(q = t.score,df = N -1))    # calculating the p-value of the test statistic
print( p.score )
# conducting a t-test using 'lsr' function
oneSampleTTest(x = grades,mu = mu.null)
# <codecell> Independent Samples t test (Student t test); assume equal population variance
str( harpo )
head( harpo )
independentSamplesTTest(    # using a function from 'lsr' package
  formula = grade ~ tutor,
  data = harpo,
  var.equal = TRUE
)
# <codecell> Indpendent Samples t test (Welch's t test); assume unequal population variance
independentSamplesTTest(
  formula = grade ~ tutor,
  data = harpo
)
# <codecell> paired sample t tests
str(chico)
head(chico)
describe(chico)    # from psych library
chico$improvement <- chico$grade_test2 - chico$grade_test1
head(chico)
hist(chico$improvement)
ciMean(chico$improvement)
oneSampleTTest(chico$improvement,mu = 0)
pairedSamplesTTest(
  formula ~ grade_test1 + grade_test2,
  data = chico
)
# testing with long data
chico2 <- wideToLong(data = chico,within = "time")
chico2 <- sortFrame( chico2,id )
head( chico2 )
pairedSamplesTTest(
  formula = grade ~ time,
  data = chico2,
  id = "id"
)
# <codecell> one-sided tests
oneSampleTTest(
  x = grades,
  mu = 67.5,
  one.sided = "greater"
)
oneSampleTTest(
  x = grades,
  mu = 67.5,
  one.sided = "lesser"
)
independentSamplesTTest(
  formula = grade ~ tutor,
  data = grade,
  one.sided = "Anastasia"    # specify which sample mean you think is higher
)
pairedSamplesTTest(
  formula = ~ grade_test1 + grade_test2,
  data = chico,
  one.sided = grade_test2    # specify which sample mean you think is higher
)
pairedSamplesTTest(
  formula = grade ~ time,
  data = chico2,
  id = "id",
  one.sided = "test2"    # specify which sample mean you think is higher
)
pairedSamplesTTest(
  formula = grade ~ time + (id),
  data = chico2,
  one.sided = "test2"    # specify which sample mean you think is higher
)
# <codecell> using the t.test() function
t.test(x = grades, mu = 67.5)    # one sample test
t.test(formula = grade ~ tutor, data = harpo)    # independent samples t test (Welch's test)
t.test(formula = grade ~ tutor, data = harpo,var.equal = TRUE)    # students indendent sample t test
t.test(x = chico$grade_test1,y = chico$grade_test2,paired = TRUE)    # paired t test
# <codecell> effect size
cohensD( x = grades, mu = 67.5)
print( (mean(grades) - 67.5) / sd(grades) )    # effect size for one sample t test
cohensD( formula = grade ~ tutor, data = harpo, method = "pooled")
cohensD( formula = grade ~ tutor, data = harpo, method = "unequal")
# <codecell> checking the normality of a sample
normal.data <- rnorm(n = 100)
hist( normal.data )
qqnorm( normal.data )
shapiro.test( normal.data )
# <codecell> test for non-normal data - Wilcoxon  Test
head( awesome )
wilcox.test( formula = scores ~ group, data = awesome )
