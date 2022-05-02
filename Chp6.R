# Chapter 6: Plotting <codecell> basic plot
Fibonacci <- c(1, 1, 2, 3, 5, 8, 13)
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
load("data/aflsmall.Rdata")
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
