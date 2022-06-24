# Chapter Sixteen: Factorial ANOVA <codecell> setup
library("lsr")
load("data/clinicaltrial.Rdata")
library("sciplot")
library("effects")
library("car")
load('data/rtfm.Rdata')
load('data/coffee.Rdata')
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
Effect( c("attend","reading"), anova.model)
regression.model <- lm( grade ~ attend + reading, data = rtfm.1 )
summary( regression.model )
anova( regression.model )
regression.model.2 <- lm( grade ~ attend + reading, data = rtfm.2 )
summary( regression.model.2 )
rtfm.3 <- rtfm.2
`rtfm.3`
rtfm.3$reading <- relevel( x = rtfm.3$reading, ref = 'yes' )
rtfm.3$attend <- relevel( x = rtfm.3$attend, ref = 'yes' )
regression.model.3 <- lm( grade ~ attend + reading, data = rtfm.3 )
clin.trial.2 <- expandFactors( clin.trial )    # expanding non-binary factors into contrasts
# <codecell> different ways to specify contrasts
contr.treatment( n = 5 )    # creates a contrast matrix for a facor with five levels
contr.helmert( n = 5 )
contr.sum( n - 5 )
options( 'contrasts' )    # viewing default contrast settings
contrasts( clin.trial$drug ) <- contr.sum( n = 3 )    # setting contrast matrix for a specific factor
contrasts( clin.trial$drug )
my.contrasts <- list( drug = contr.helmert , therapy = contr.helmert )    # setting contrasts for a specific analysis
mod <- aov( mood.gain ~ drug * therapy, data = clin.trial )
mod$contrasts
# <codecell> post-hoc tests
TukeyHSD( model.2 )
TukeyHSD( model.3 )    # including pairwise comparison of interactions
# <codecell> factorial ANOVA: unbalanced designs
some( coffee )    # pick a few random observations (car package function)
aggregate( babble ~ sugar + milk, coffee, mean )
xtabs( ~ milk + sugar, coffee )
mod <- lm( babble ~ sugar + milk + sugar:milk, data = coffee )    # create the linear model for the unbalanced data
anova( mod )
mod.1 <- lm( babble ~ 1 , coffee )    # simplest model
mod.2 <- lm( babble ~ sugar , coffee )
mod.3 <- lm( babble ~ sugar + milk , coffee )
mod.4 <- lm( babble ~ sugar + milk + sugar:milk , coffee )    # most complex model
anova( mod.1,mod.2,mod.3,mod.4 )    # comparing all models
mod <- lm( babble ~ milk + sugar + sugar:milk, data = coffee )
anova( mod )    # order of factors affects output
# type III Sum of Squares
mod <- lm( babble ~ sugar * milk, coffee )
Anova( mod , type = 3 )    # using a function from the 'car' package to specify the type of Sum of Squares used
my.contrasts <- list( 'milk' = contr.Helmert, 'sugar' = contr.Helmert )
mod.H <- lm( babble ~ sugar * milk, coffee , contrasts = my.contrasts )
Anova( mod.H , type = 3 )    # specifying contrasts in the model
# specifying random contrasts which sum to zero
random.contrasts <- matrix( rnorm( 6 ), 3, 2 )
random.contrasts[,1] <- random.contrasts[,1] - mean( random.contrasts[,1] )
random.contrasts[,2] <- random.contrasts[,2] - mean( random.contrasts[,2] )
print( random.contrasts )
contrasts( coffee$sugar ) <- random.contrasts
contrasts( coffee$milk ) <- contr.helmert(2)
mod.R <- lm(babble ~ sugar * milk, coffee )
Anova( mod.R, type = 2 )
# type II Sum of Squares
mod <- lm( babble ~ milk * sugar, coffee )
Anova( mod, type = 2 )
# Effect Size
etaSquared( mod , type = 2 )    # a function from the 'lsr' package
es <- etaSquared( mod , type = 2 , anova = TRUE )
sum( es[,"eta.sq"] )    # eta squared values do not sum to 1
SS.tot <- sum( (coffee$babble - mean( coffee$babble ) ) ^2 )
type.I.sum <- 0.9561 + 3.5575 + 5.9439 + 3.1625    # pulled from ANOVA table
print( SS.tot , type.I.sum )
print( type.I.sum )    # they are the same
type.II.sum <- 0.9561 + 3.0696 + 5.9439 + 3.1625    # pulled from ANOVA table
print( SS.tot )
print( type.II.sum )    # not the same
