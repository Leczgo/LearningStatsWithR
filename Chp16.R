# Chapter Sixteen: Factorial ANOVA <codecell> setup
library("lsr")
load("data/clinicaltrial.Rdata")
library("sciplot")
library("effects")
library("car")
load('data/rtfm.Rdata')
# <codecell> Balanced, no interactions
model.1 <- aov( mood.gain ~ drug, data = clin.trial )
summary( model.1 )
model.2 <- aov( mood.gain ~ drug + therapy, data = clin.trial )
summary( model.2 )
xtabs( ~ drug + therapy, clin.trial )
# calculate group means
aggregate( mood.gain ~ drug + therapy , data = clin.trial, mean)
drug.means <- aggregate( mood.gain ~ drug , data = clin.trial, mean )[,2]
therapy.means <- aggregate( mood.gain ~ therapy, data = clin.trial, mean )[,2]
grand.mean <- mean( clin.trial$mood.gain )
# calculating sum of squares
SS.drug <- (3 * 2) * sum( (drug.means - grand.mean)^2 )
SS.therapy <- (3 * 3) * sum( (therapy.means - grand.mean)^2 )
SS.tot <- sum( (clin.trial$mood.gain - grand.mean)^2 )
SS.res <- SS.tot - (SS.drug + SS.therapy)
# <codecell> Balanced, no interactions
interaction.model <- aov( mood.gain ~ drug + therapy + drug:therapy, data = clin.trial )
summary( interaction.model )
# <codecell> Effect size
etaSquared( model.2 )
eff <- effect( term = "drug*therapy", mod = interaction.model )
summary( eff )    # give confidence intervals
# <codecell> testing assumptions
leveneTest( interaction.model )
leveneTest( model.2 )    # will output an error because the tested model is not 'saturated'
resid <- residuals(model.2)
hist( resid )    # visualize distribution of residuals
qqnorm( resid )    # draw QQ plot
shapiro.test( resid )    # perform a shapiro test to check normality of residuals
# <codecell> F-test as a model comparison
anova( model.1, interaction.model )
# <codecell> ANOVA as a linear model
rtfm.1
rtfm.2    # levels coded as factors
xtabs( ~ attend + reading, rtfm.2 )
anova.model <- aov( grade ~ attend + reading , data = rtfm.2 )
summary( anova.model )
