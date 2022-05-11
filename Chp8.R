# Chapter Eight: Programming Basics <codecell> initial setup
library("lsr")
# <codecell> loops
x <- 0
while (x < 1000) {
  x <- x + 17
}
print(x)
for (i in 1:3) {
  print("Hello")
}
words <- c("it","was","the","dirty","end","of","winter")
for ( w in words ) {
  w.length <- nchar(j)
  W <- toupper(w)
  msg <- paste(W,"has",w.length,"letters.")
  print( msg )
}
## loop example: mortgage
month <- 0
balance <- 30000
payments <- 1600
interest <- 0.05
total.paid <- 0
monthly.multiplier <- (1 + interest) ^ (1/12)
while (balance > 0) {
  month <- month + 1
  balance <- balance * monthly.multiplier
  balance <- balance - payments
  total.paid <- total.paid + payments
  cat("month",month,": balance",round(balance),"\n")
}
cat("total payments made",total.paid,"\n")
# <codecell> conditional statements
today <- Sys.Date()
day <- weekdays( today )
if (day == "Monday") {
  print( "I don't like Mondays." )
} else {
  print( "I\'m a happy little automoton.")
}
# <codecell> functions
quadruple <- function(x) {
  y <- x * 4
  return(y)
}
my.var <- quadruple(10)
print(my.var)
# <codecell> implicit loops
words <- c("along","the","loom","of","the","land")
sapply(X = words,FUN = nchar)
gender <- c("male","male","female","female","male")
age <- c(10,12,9,11,13)
tapply(X = age,INDEX = gender,FUN = mean)
