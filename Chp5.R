#Chapter 5 code samples
#written in ATOM using w/ Hydrogen Package
# <codecell> load data & packages
load("data/aflsmall.Rdata")
load("data/clinicaltrial.Rdata")
load("data/parenthood.Rdata")
load("data/effort.Rdata")
load("data/work.Rdata")
library("lsr")
library( "psych" )
# <codecell> exploratory analysis
who()
print(afl.margins)
# <codecell> analysis of means
sum(afl.margins)
sum(afl.margins[1:5])
sum(afl.margins[1:5]) / 5
mean(x = afl.margins)
mean(afl.margins[1:5])
# <codecell> analysis of medians
sort(x = afl.margins)
median(x = afl.margins)
# <codecell> trimmed means
dataset <- c(-15,2,3,4,5,6,7,8,9,12)
mean(dataset)
mean(x = dataset,trim = 0.1)
mean(x = afl.margins, trim = 0.05)
# <codecell> MODEs
table(afl.finalists)
modeOf(x = afl.finalists)
maxFreq(x= afl.finalists)
# <codecell> Range
max(afl.margins)
min(afl.margins)
range(afl.margins)
# <codecell> Interquartile Range
quantile(x = afl.margins, probs = 0.5)
quantile(x = afl.margins, probs = c(0.25,0.75))
IQR(afl.margins)
# <codecell> mean absolute deviation
X <- c(56,31,56,8,32)
X.bar <- mean(X)
AD <- abs(X - X.bar)
AAD <- mean(AD)
print(AAD)
aad(X)
# <codecell> variance
mean( (X - mean(X) ) ^2 )
var(X)
mean( (afl.margins - mean(afl.margins) ) ^2 )
var(afl.margins)
sum( (X - mean(X) ) ^2 )/4
# <codecell> standard deviation
sd(afl.margins)
# <codecell> median absolute deviation
median( abs( X - median(X) ) )
mad(x = afl.margins, constant = 1)
# <codecell> skew & kurtosis
skew(afl.margins)
kurtosi(afl.margins)
# <codecell> summarising data
summary(object = afl.margins)
blowouts <- afl.margins > 50
summary(object = blowouts)
summary(object = afl.finalists)
f2 <- as.character(afl.finalists)
summary(object = f2)
summary(object = clin.trial)
describe( x = clin.trial )
# <codecell> describing by group
describeBy(x = clin.trial, group = clin.trial$therapy)
by(data = clin.trial, INDICES = clin.trial$therapy, FUN = describe)
by(data = clin.trial, INDICES = clin.trial$therapy, FUN = summary)
aggregate(formula = mood.gain ~ drug + therapy,
data = clin.trial,
FUN = mean)
aggregate(mood.gain ~ drug + therapy, clin.trial, sd)
# <codecell> standard scores
pnorm(3.6)
# <codecell> correlations
cor(x = parenthood$dan.sleep,y = parenthood$dan.grump)
cor(x = parenthood)
cor(effort$hours,effort$grade)
hours.rank <- rank(effort$hours)
grade.rank <- rank(effort$grade)
cor(effort$hours,effort$grade,method = "spearman")
correlate(work)
correlate(work,corr.method = "spearman")
