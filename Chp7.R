# Chapter 7: Pragmatic Matters
# <codecell> loading workspace
fibonacci <- c(1,1,2,3,5,8)
load("data/nightgarden.Rdata")
load("data/nightgarden2.Rdata")
load("data/cakes.Rdata")
load("data/repeated.Rdata")
load("data/likert.Rdata")
library("lsr")
# <codecell> tabulating data
who()
print(speaker)
print(utterance)
table(speaker) # creates frequency table
table(speaker,utterance) # cross-tabulation
itng <- data.frame(speaker,utterance)
table(itng)
xtabs(formula = ~ speaker + utterance, data = itng)
itng.table <- table(itng)
prop.table(itng.table) # proportion table by total value
prop.table(itng.table, margin = 1) # proportion table by rows value
prop.table(x = itng.table, margin = 2) # proportion table by columns value
# <codecell> transforming & recoding a variable
likert.raw
likert.centered <- likert.raw - 4 # transforming a vector of variables
opinion.strength <- abs( likert.centered )
opinion.dir <- sign( likert.centered )
# cutting a numeric variable into categories
age <- c(60, 58, 24, 26, 34, 42, 31, 30, 33, 2, 9)
age.breaks <- seq(from = 0, to = 60, by = 20)
age.labels <- c("young","adult","older")
age.group <- cut(x = age, breaks = age.breaks, labels = age.labels)
table(age.group)
age.group3 <- quantileCut(x = age,n = 3)
table(age.group3)
# <codecell> a few more mathematical operations
round(4.357892,digits = 2) # rounding to two decimal points
signif(0.001045, digits = 2) # rounding to two signficant figures
42 %/% 10 # integer division
42 %% 10 # modulus division
-42 %/% 10
-42 %% 10
# logarithms and exponentials
log10(1000)
exp(3)
log(exp(3))
# <codecell> extracting a subset of a vector
is.MP.speaking <- speaker == "makka-pakka"
utterance[is.MP.speaking]
utterance[speaker == "makka-pakka"]
utterance %in% c("pip","oo")
speaker[utterance %in% c("pip","oo")]
utterance[-(2:3)]
speech.by.character <- split(x = utterance,f = speaker) #using the split function
speech.by.character$`makka-pakka`
importList(speech.by.character) # using lsr function to extract list values
# <codecell> extracting a subset of a data frame
df <- subset(x = itng, # pick data frame
    subset = speaker == "makka-pakka", # select rows
    select = utterance) # select columns
garden[4:5,1:2] # selecting using brackets[rows,columns] and row/column numbers
garden[c("case.4","case.5"),c("speakder","utterance")] # using row/column names
# <codecell> sorting, flipping, and merging data
numbers <- c(1,2,3,4)
sort(x = numbers)
sort(x = numbers, decreasing = TRUE)
text <- c("aardvark","zebra","swing")
sort(text)
fac <- factor(text)
sort(fac)
fac <- factor(text,factors = c("aardvark","zebra","swing")) #defining factors
sort(fac) #will sort by factor level by default
sortFrame(x = garden, speaker, -line) # using lsr function to sort a data frame
cake.1 <- c(100,80,0,0,0)
cake.2 <- c(100,100,90,30,10)
cake.mat1 <- cbind(cake.1,cake.2) # bind into 5x2 matrix
cake.mat2 <- rbind(cake.1,cake.2) # bind into a 2x5 matrix
rowCopy(x = fibonacci,times = 3) # using an lsr function to rbind multiple copies
colCopy(x = fibonacci,times = 3) # using an lsr runction to cbind multiple copies
# transpose a matrix and a data frame
class(cakes) # it is a matrix
cakes.flipped <- t(cakes) #transposed the matrix (can also transpose data frames)
itng.flipped <- tFrame(itng) #using an lsr function to transpose a dataframe
# <codecell> working with string data
animals <- c("cat","dog","kangaroo","whale")
strtrim(x = animals, width = 3) # shortening all strings in a vector
substr(x = animals,start = 2,stop = 3)
# joining text
paste("hello","world")
paste("hello","world",sep = ".")
hw <- c("hello","world")
ng <- c("nasty","government")
paste(hw,ng)
paste(hw,ng,sep = ".",collapse = ":::")
# splitting text
monkey <- "It was the best of times. It was the worst of times."
monkey.1 <- strsplit(x = monkey,split = " ",fixed = TRUE)
text <- c("lIfe","Impact")
# modifying text
tolower(x = text)
old.text <- "albino"
chartr(old = "aln",new = "lid",x = old.text)
# logical operators with text
"cat" < "dog" #cat comes before dog alphabetically
"anteater" < "ZEBRA" # returns FALSE because all uppercase occur before any lowercase
# using the cat() function
cat(hw,ng)
cat(hw,ng,sep = " ")
print("hello\nworld")
cat("hello\nworld")
PJ <- "P.J. O\'Rourke says, \"Yay, money!\". It\'s a joke, but no-one laughs."
print(PJ)
print.noquote(PJ)
cat("xxxx\boo")    # backspace deletes last 'x' before printing
cat("xxxx\too")    # tab space adds a tab
cat("xxxx\noo")    # adds a new line
cat("xxxx\roo")
# matching & substituting (grep, gsub, etc)
beers <- c("little creatures","sierra nevada","coopers pale")
grep(x = beers,pattern = "er",fixed = TRUE)
grep(pattern = "er",x = beers,fixed = TRUE,value = TRUE)
gsub(pattern = "a", replacement = "BLAH",x = beers,fixed = TRUE)    # replaces all 'a' values
sub(pattern = "a",replacement = "BLAH",x = beers,fixed = TRUE)    # replaces only the first 'a' in each string
grep(x = beers,pattern = "[a,e,i,o,u]s",value = TRUE)    # using a general pattern
# <codecell> coercing data classes
x <- "100"
class(x)
x + 1
x <- as.numeric(x)
class(x)
x + 1
# <codecell> other useful data structures
row.1 <- c(2,3,1)
row.2 <- c(5,6,7)
M <- rbind(row.1,row.2)    # bind into a matrix
print( M )
M[1,2] <- "text"    # accessing and modifying a matrix value
class(M[1])    # accessing using single index notation; returns 'character' because all entries were coerced to characters
# ordered factors
likert.ordinal <- factor(x = likert.raw,
    levels = seq(7,1,-1),
    ordered = TRUE)
print( likert.ordinal )
# time and date data
today <- Sys.Date()
print( today )
today + 1    # modifying the date
today + 365
weekdays(today)
# <codecell> miscellaneous topics
# problems with float values
0.1 + 0.2 - 0.3
all.equal(0.1 + 0.2,0.3)    # using a function that allows for small rounding
