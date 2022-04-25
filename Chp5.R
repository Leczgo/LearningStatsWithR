#Chapter 5 code samples
#written in ATOM using w/ Hydrogen Package
# <codecell> load data & packages
load("data/aflsmall.Rdata")
library("lsr")
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
