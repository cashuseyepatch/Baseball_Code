##  Section 2.3 vectors.  

W <- c(8,21,15,21,21,22,14)

L <- c(5,10,12,14,17,14,19)

Win.Pct <- 100 * W/(W+L)

Win.Pct

Year <- seq(1946,1952)
Year

Age <- Year - 1921

x11()

plot(Age, Win.Pct)

mean(Win.Pct)

100 * sum(W)/(sum(W) + sum(L))

sort(W)

cumsum(W)

summary(Win.Pct)

W[c(1,2,5)]

W[1:4]

W[-c(1,6)]

Win.Pct > 60

(W>20) & (Win.Pct>60)

Win.Pct==max(Win.Pct)

Year[Win.Pct==max(Win.Pct)]

Year[W+L>30]

##  Section 2.4 Objects and Containers in R.  

##  2.4.1 Character data and matrices.  

NL <- c("FLA", "STL", "HOU", "STL", "COL",
        "PHI", "PHI", "SFG", "STL", "SFG")
AL <- c("NYY","BOS","CHW","DET","BOS",
        "TBR","NYY","TEX","TEX","DET")
Winner <- c("NL","AL","AL","NL","NL","NL","AL","NL","NL","NL")
N.Games <- c(6,4,4,5,4,5,6,5,7,4)
Year <- 2003:2012

results <- matrix(c(NL,AL),10,2)
results

dimnames(results)[[1]] <- Year
dimnames(results)[[2]] <- c("NL Team","AL Team")
results

table(Winner)

barplot(table(Winner))

##  2.4.2 Factors.  

NL2 <- factor(NL, levels=c("FLA","PHI","HOU","STL","COL","SFG"))

str(NL2)

table(NL2)

##  2.4.3 Lists.  

World.Series <- list(Winner=Winner, Number.Games=N.Games, Seasons="2003 to 2012")

World.Series$Number.Games

World.Series[[2]]

World.Series["Number.Games"]

##  Collection of R Commands.  

##  2.5.1 R scripts.  

N.Games <- c(6,4,4,5,4,5,6,5,7,4)
Winner <- c("NL","AL","AL","NL","NL","NL","AL","NL","NL","NL")
table(Winner)
barplot(table(Winner))
by(N.Games, Winner, summary)

##  2.5.2 R functions.  

hr.rates <- function(age, hr, ab){
    rates <- round(100*hr/ab,1)
    list(x=age,y=rates)
}

HR <- c(13,23,21,27,37,52,34,42,31,40,54)
AB <- c(341,549,461,543,517,533,474,519,541,527,514)
Age <- 19:29
hr.rates(Age,HR,AB)

x11()
plot(hr.rates(Age,HR,AB))

##  2.6 Reading and Writing Data in R.

##  2.6.1 Importing data from a file.  

##  2.6.2 Saving datasets.  

HR <- c(13,23,21,27,37,52,34,42,31,40,54)
AB <- c(341,549,461,543,517,533,474,519,541,527,514)
Age <- 19:29
HR.Rates <- hr.rates(Age,HR,AB)
Mantle <- cbind(Age,HR,AB,Rates=HR.Rates$y)

write.csv(Mantle,"mantle.csv", row.names=FALSE)

## 2.7 Data Frames

## 2.7.1 Inrtoduction

spahn[1:3,1:10]

spahn[1,]

spahn[1:10, c("Age","W","L","ERA")]

summary(spahn$ERA)

spahn$Age[spahn$ERA == min(spahn$ERA)]

##  2.7.2 Manipulations with data frames.  

spahn$FIP <- with(spahn, (13*HR + 3*BB - 2*SO)/IP)

pos <- order(spahn$FIP)
head(spahn[pos, c("Year","Age","W","L","ERA","FIP")])

spahn1 <- subset(spahn, Tm=="BSN" | Tm == "MLN")

spahn1$Tm <- factor(spahn1$Tm, levels=c("BSN","MLN"))

by(spahn1[,c("W.L","ERA","WHIP","FIP")],spahn1$Tm,summary)

##  2.7.3 Merging and selecting from data frames.  

NLbatting <- read.csv("NLbatting.csv")
ALbatting <- read.csv("ALbatting.csv")
batting <- rbind(NLbatting,ALbatting)

NLpitching <- read.csv("NLpitching.csv")
NL <- merge(NLbatting, NLpitching, by="Tm")

NL.150 <- subset(NLbatting, HR > 150)

##  2.8 Packages.  

install.packages("Lahman")

library(Lahman)

?Batting

##  2.9 Splitting, Applying, and Combining Data.  

Batting <- read.csv("Batting.csv")

Batting.60 <- subset(Batting, yearID >= 1960 & yearID <= 1969)

compute.hr <- function(pid){
    d <- subset(Batting.60, playerID==pid)
    sum(d$HR)
}

players <- unique(Batting.60$playerID)
S <- sapply(players,compute.hr)

R <- sapply(players, compute.hr)

R <- data.frame(Players=players, HR=S)
R <- R[order(R$HR, decreasing=TRUE),]
head(R)

##  The below sees if I can create Batting.60 without using the subset
##  funciton.  

##  Vec.60 is a logical vector of positions within Batting that says
##  whether yearID is within the 60s.  

Vec.60 <- (Batting$yearID>=1960) & (Batting$yearID<=1969)

Batting.60.test <- Batting[Vec.60, ]

identical(Batting.60, Batting.60.test)

Batting.60.test2 <- Batting[(Batting$yearID>=1960) & (Batting$yearID<=1969),]

identical(Batting.60, Batting.60.test2)

##  2.9.2 Using the ddply in the plyr package.  

library(plyr)
dataframe.AB <- ddply(Batting, .(playerID), 
                      summarize, Career.AB=sum(AB, na.rm=TRUE))

Batting <- merge(Batting, dataframe.AB, by="playerID")

Batting.5000 <- subset(Batting, Career.AB >= 5000)

Batting.5000.test <- Batting[Batting$Career.AB >= 5000,]

identical(Batting.5000, Batting.5000.test)

##  The following function, 'ab.hr.so' calculates the career AB, HR, and SO,
##  for a single player.  The input is the data frame d containing the 
##  statistics for one player and the output is a data frame with the career
##  AB, HR, and SO.  

ab.hr.so <- function(d){
    c.AB <- sum(d$AB, na.rm=TRUE)
    c.HR <- sum(d$HR, na.rm=TRUE)
    c.SO <- sum(d$SO, na.rm=TRUE)
    data.frame(AB=c.AB, HR=c.HR, SO=c.SO)
}

##  To illustrate the use of ab.hr.so, we extraxt Hank Aaron's batting
##  statistics and apply this function on Aaron's data frame aaron. 

aaron <- Batting.5000[Batting.5000$playerID=="aaronha01",]
ab.hr.so(aaron)

##  To apply this function to each batter and collect the results, we again
##  use the function ddply.  The arguments are the data frame Batting.5000
##  to split, the splitting variable playerID, and the function ab.hr.so
##  to apply to each part.  

d.5000 <-ddply(Batting.5000, .(playerID), ab.hr.so)

head(d.5000)

x11()
with(d.5000, plot(HR/AB,SO/AB))
with(d.5000, lines(lowess(HR/AB,SO/AB)))
