# Chapter 15: Linear Regression <codecell> initial setup
library("lsr")
library("lmtest")
library("car")
load("data/parenthood.Rdata")
# <codecell> linear model formula
regression.1 <- lm(dan.grump ~ dan.sleep , data = parenthood)
print( regression.1 )
# <codecell> multiple linear regression
regression.2 <- lm(dan.grump ~ dan.sleep + baby.sleep, data = parenthood)
print( regression.2 )
# <codecell> the R^2 Value
X <- parenthood$dan.sleep
y <- parenthood$dan.grump
predictor <- -8.94 * X + 125.97
SS.resid <- sum( (y - predictor)^2 )
print( SS.resid )
SS.tot <- sum( (y - mean(y))^2 )
print( SS.tot )
R.squared <- 1 - (SS.resid / SS.tot)
print(R.squared)
r <- cor( X , y )
print( r ^ 2 )
# <codecell> hypothesis testing for regression
summary( regression.2 )
# <codecell> testing significance of a correlation
summary( regression.1 )
cor.test( x = parenthood$dan.sleep, y = parenthood$dan.grump )
correlate( parenthood, test = TRUE )    # using a function from 'lsr' package
# <codecell> regarding regression coefficients
confint( object = regression.2 , level = 0.99 )    # print confidence intervals for coefficients
standardCoefs( regression.2 )    # using a function from 'lsr' package
# <codecell> checking Assumptions
# residuals
resid <- residuals( object = regression.2 )
stand.resid <- rstandard( model = regression.2 )
student.resid <- rstudent( model = regression.2 )
# anomalous values
hats <- hatvalues( model = regression.2 )    # extracting hat values
val.influence <- cooks.distancs( model = regression.2 )
plot( x = regression.2 )    # will plot all 6 charts of interest for checking model
plot( x = regression.2, which = 4 )    # will plot Cook's distance of each observation
lm( formula = dan.grump ~ dan.sleep + baby.sleep, data = parenthood, subset = -64 )    # removes anomalous datapoint
# checking residual normality
hist( x = resid, xlab = "Value of Residuals", main = "", breaks = 20 )
plot( x = regression.2 , which = 2 )    # will give qqplot
qqplot( regression.2 )
# checking linearity relationship
yhat.2 <- fitted.values( object = regression.2 )
plot( x = yhat.2, y = parenthood$dan.grump, xlab = "Fitted Values", ylab = "Observed Values" )
plot( x = regression.2 , which = 1 )
residualPlots( model = regression.2 )    # using a function from the 'car' package to plot residuals
# checking homogeneity of variance
plot( x = regression.2, which = 3 )
ncvTest( regression. 2 )    # using a function from the 'car' package to test for a relationship with residuals and fitted Values
# colinearity
vif( mod = regression.2 )    # checking variance inflation factors
# <codecell> model selection
# backward selection
full.model <- lm( dan.grump ~ dan.sleep + baby.sleep + day, data = parenthood)
step( object = full.model, direction = "backward" )
