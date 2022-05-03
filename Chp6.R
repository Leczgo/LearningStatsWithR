# Chapter 6: Plotting <codecell> basic plot
Fibonacci <- c(1, 1, 2, 3, 5, 8, 13)
load("data/aflsmall.Rdata")
load("data/aflsmall2.Rdata")
load("data/parenthood.Rdata")
library("car")
# first plot
plot(Fibonacci)
# second plot w/ labels
plot(x = Fibonacci,
  main = "You specify the title using the 'main' arument",
  sub = "The subtitle appears here! (Use the 'sub argument for this')",
  xlab = "The x-axis label is xlab",
  ylab = "The y-axis label is ylab")
# third plot with extra parameters
plot(Fibonacci,
  main = "The first seven Fibonacci Numbers",
  xlab = "The position in the sequence",
  ylab = "The Fibonacci Number",
  font.main = 1,
  cex.main = 1,
  font.axis = 2,
  col.lab = "gray50")
# fourth plot with extra characterization parameters
plot(Fibonacci,
  type = "b",
  col = "blue",
  pch = 19,
  cex = 5,
  lty = 2,
  lwd = 4)
# fifth plot with axes parameters
plot(Fibonacci,
  xlim = c(0,15),
  ylim = c(0,15),
  ann = FALSE,
  axes = FALSE,
  frame.plot = FALSE)
# sixth plot with extra special axes parameters
plot(Fibonacci,
  xaxt = "n",
  bty = "]",
  las = 1)
# <codecell> Histograms
hist(afl.margins) # first histogram
hist(afl.margins, breaks = 3) # histogram specifying bins
hist(afl.margins, breaks = 0:116)
# histogram with extra parameters
hist(x = afl.margins,
  main = "2010 AFL Margins",
  xlab = "Margin",
  density = 10,
  angle = 40,
  border = "gray20",
  col = "gray80",
  labels = TRUE,
  ylim = c(0,40) )
# <codecell> Stem & Leaf Plots
stem(afl.margins)
max(afl.margins)
stem(x = afl.margins, scale = 0.25)
stem(x = afl.margins, width = 20)
stem(x = afl.margins / 1000)
# Boxplots
summary(afl.margins)
boxplot(x = afl.margins,range = 100) # default range = 1.5
boxplot(x = afl.margins)
boxplot(x = afl.margins,
  xlab = "AFL Games, 2010",
  ylab = "Winning Margin",
  border = "grey50",
  frame.plot = FALSE,
  staplewex = 0,
  whisklty = 1)
boxplot(formula = margin ~ year,
  data = afl2) #plotting multiple boxplots
boxplot(formula = margin ~ year,
  data = afl2,
  xlab = "AFL Season",
  ylab = "Winning Margin",
  frame.plot = FALSE,
  staplewex = 0,
  staplecol = "white",
  boxwex = 0.75,
  boxfill = "grey80",
  whisklty = 1,
  whiskcol = "grey70",
  boxcol = "grey70",
  outcol = "grey70",
  outpch = 20,
  outcex = 0.5,
  medlty = "blank",
  medpch = 20,
  medlwd = 1.5)
# <codecell> Scatterplots
plot(x = parenthood$dan.sleep,y = parenthood$dan.grump)
plot( x = parenthood$dan.sleep,
  y = parenthood$dan.grump,
  xlab = "My sleep (hours)",
  ylab = "My grumpiness (0 - 100)",
  xlim = c(0,12),
  ylim = c(0,100),
  pch = 20,
  col = "gray50",
  frame.plot = FALSE
)
  lines(x = c(4,9.5),
    y = c(93,37)
  )
scatterplot(dan.grump ~ dan.sleep,
  data = parenthood,
  smooth = FALSE) # used cars package
# draw a matrix of scatterplots, similar to correlation matrix
pairs(parenthood)
pairs(formula = ~ dan.grump + dan.sleep + baby.sleep,
  data = parenthood)
# <codecell> Bar Charts
freq <- tabulate(afl.finalists)
print(freq)
teams <- levels(afl.finalists)
print(teams)
barplot(height = freq,
  names.arg = teams,
  las = 2)
