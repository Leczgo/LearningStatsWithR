# Chapter 14 <codecell> One-Way ANOVA
load("data/clinicaltrial.Rdata")
library("lsr")
library("gplots")
library("car")
# <codecell> Introduction
str(clin.trial)
print(clin.trial)
# exploratory data analysis
xtabs(~ drug,data = clin.trial)
aggregate(mood.gain ~ drug , data = clin.trial,FUN = mean)
aggregate(mood.gain ~ drug, data = clin.trial, FUN = sd)
plotmeans(formula = mood.gain ~ drug,
    data = clin.trial, xlab = "Drug Administered",
    ylab = "Mood Gain", n.label = FALSE)
# <codecell> How ANOVA works
outcome <- clin.trial$mood.gain
group <- clin.trial$drug
gp.means <- tapply(outcome,group,mean)
gp.means <- gp.means[group]
dev.from.group.means <- outcome - gp.means
squared.devs <- dev.from.group.means ^ 2
Y <- data.frame(group,outcome,gp.means,dev.from.group.means,squared.devs)
print(Y,digits = 2)
SSw <- sum(squared.devs)
print(SSw)
grand.mean <- mean(outcome)
gp.means <- tapply(outcome,group,mean)
dev.from.grand.means <- gp.means - grand.mean
bt.squared.devs <- dev.from.grand.means ^ 2
gp.sizes <- tapply(outcome,group,length)
bt.squared.devs <- gp.sizes * bt.squared.devs
Y.wt <- data.frame(gp.means,grand.mean,dev.from.grand.means,bt.squared.devs)
print(Y.wt)
SSb <- sum(bt.squared.devs)
print(SSb)
df.w <- 18 - 3
df.b <- 3 - 1
MSb <- SSb / df.b
MSw <- SSw / df.w
print(MSb)
print(MSw)
F <- MSb / MSw
print( F )
pf(F,df1 = df.b,df2 = df.w,lower.tail = FALSE)
# <codecell> running ANOVA in R
aov( formula = mood.gain ~ drug , data = clin.trial )
my.anova <- aov(formula = mood.gain ~ drug,data = clin.trial)
class(my.anova)
names(my.anova)
summary(my.anova)
# <codecell> Effect Size
SSt <- SSb + SSw
eta.squared = SSb / SSt
print( eta.squared )
etaSquared( x = my.anova )    # printing effect size using a function in 'lsr' package
# <codecell> multiple comparisons and post-hoc tests
anxifree <- with(clin.trial,mood.gain[drug == "anxifree"])
placebo <- with(clin.trial,mood.gain[drug == "placebo"])
t.test(anxifree,placebo,var.equal = TRUE)    # testing one group pair using  a t test
t.test(    # another way of testing a specific group pairing
    formula = mood.gain ~ drug,
    data = clin.trial,
    subset = drug %in% c("placebo","anxifree"),
    var.equal = TRUE
)
pairwise.t.test(
    x = clin.trial$mood.gain,    # outcome variable
    g = clin.trial$drug,    # grouping variable
    p.adjust.method = "none"
)
posthocPairwiseT(x = my.anova,p.adjust.method = "none")
# corrections of pairwise t tests
posthocPairwiseT(x = my.anova,p.adjust.method = "bonferroni")
posthocPairwiseT(x = my.anova,p.adjust.method = "holm")    # the default method
# <codecell> testing homogeneity of variance (homoscedacity)
leveneTest(my.anova)    # default conducts Brown-Forscythe test
leveneTest(my.anova,center = mean)    # specify Leven Test by using mean as central measure
# <codecell> removing homogeneity of variance assumption
oneway.test( mood.gain ~ drug, data = clin.trial )    # Welch test for ANOVA
# <codecell> checking normality assumption
my.anova.residuals <- residuals( object = my.anova )    # extracting residuals
hist( my.anova.residuals )
qqplot( my.anova.residuals )
shapiro.test( my.anova.residuals )
# <codecell> removing normality assumption: kruskal-wallis rank sum test
kruskal.test(formula = mood.gain ~ drug, data = clin.trial )
kruskal.test(x = clin.trial$mood.gain,g = clin.trial$drug)
