# Chapter Seventeen <codecell> setup
load('data/chapek9.Rdata')
load( 'data/harpo.Rdata' )
load( 'data/chico.Rdata' )
library('lsr')
library( 'BayesFactor' )
load( 'data/parenthood.Rdata' )
load( 'data/clinicaltrial.Rdata' )
# <codecell> Bayesian Analysis using contingency tables
head(chapek9)
crosstab <- xtabs( ~ species + choice, chapek9 )
crosstab
associationTest( ~ species + choice , chapek9 )
contingencyTableBF( crosstab , sampleType = 'jointMulti' )
contingencyTableBF( crosstab , sampleType = 'poisson' )    # Bayes Factor is different due to different sampling plan
contingencyTableBF( crosstab , sampleType = 'indepMulti' , fixedMargin = 'rows' )
toys <- data.frame( stringsAsFactors = FALSE ,
  gender = c('girl','boy') ,
  pink = c(8,2) , blue = c(2,8) )
contingencyTableBF( toys , sampleType = 'hypergeom' )
# <codecell> Bayesian t-tests
head( harpo )    # independent t-test
independentSamplesTTest( formula = grade ~ tutor , data = harpo, var.equal = TRUE )
ttestBF( formula = grade ~ tutor , data = harpo )
head( chico )    # paired sample t-test
ttestBF( x = chico$grade_test1,y = chico$grade_test2,paired = TRUE )
# <codecell> Bayesian Regression
head( parenthood )
model <- lm( dan.grump ~ dan.sleep + day + baby.sleep , data = parenthood )
summary( model )
BFmodel <- regressionBF( formula = dan.grump ~ dan.sleep + day + baby.sleep, data = parenthood )
head( BFmodel , n = 3 )    # select only the best three models
head( BFmodel / max(BFmodel) , n = 3 )    # compare the best three models against the very best model
BFmodel[1] / BFmodel[4]    # compare specifically the first model listed with the fourth model listed
# show analysis of selected models and the BF from omitting each term
regressionBF( formula = dan.grump ~ dan.sleep + baby.sleep, data = parenthood, whichmodels = 'top' )
# <codecell> Bayesian ANOVA
head( clin.trial )
model <- aov( formula = mood.gain ~ drug * therapy, data = clin.trial )
Anova( model )
BFaov <- anovaBF( formula = mood.gain ~ drug * therapy, data = clin.trial )
BFaov / max( BFaov )    # comparing all models agains the best model
