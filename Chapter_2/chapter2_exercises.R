##  2.12 Exercises.  

##  1. (Top Base Stealers in the Hall of Fame)
##  The following table gives the numer of stolen bases (SB), the number of
##  times caught stealing (CS), and the number of games played (G) for nine
##  players currently inducted in the hall of fame.  

##  (a).  In R, place the stolen base, caught stealing, and game counts in 
##  the vectors SB, CS, and G.  

SB <- c(1406, 938, 897, 741, 738, 689, 506, 504, 474)
CS <- c(335,307,212, 195, 109, 162, 136, 131, 114)
G <- c(3081, 2616, 3034, 2826, 2476, 2649, 2599, 2683, 2379)

##  (b).  For all players, compute the number of stolen base attempts
##  SB + CS and store in the vector SB.Attempt.  

SB.Attempt <- SB + CS

##  (c).  For all players compute the success rate Success.Rate = 
##  SB/SB.Attempt.  

Success.Rate <- SB/SB.Attempt

##  (d).  Compute the number of stolen bases per game SB.Game = SB/Game

SB.Game = SB/G

##  (e).  Construct a scatterplot of the stolen bases per game against the
##  success rates.  

x11()
plot(Success.Rate,SB.Game)

##  I added the following.  

names <- c("Rickey Henderson", "Lou Brock", "Ty Cobb", "Eddie Collins", 
           "Max Carey", "Joe Morgan", "Luis Aparicio", "Paul Molitor",
           "Roberto Alomar")

stolenbase <- data.frame(names, SB, CS, G, SB.Attempt, Success.Rate, SB.Game)

plot(stolenbase$Success.Rate,stolenbase$SB.Game)
with(stolenbase, text(Success.Rate, SB.Game, names, pos=4))

##  2. (Character, Factor, and Logical Variables in R)
##  Suppose one records the outcomes of a batter in ten plate appearances:
##  Single, Out, Out, Single, Out, Double, Out, Walk, Out, Single.  

#  (a).

outcomes <- c("Single", "Out", "Out", "Single", "Out", "Double", "Out", 
              "Walk", "Out", "Single")

#  (b).

table(outcomes)

#  (c).  

f.outcomes <- factor(outcomes, levels=c("Out","Walk","Single","Double"))

table(f.outcomes)

#  (d).  

outcomes == "Walk"

sum(outcomes=="Walk")

##  3.  (Pitchers in the 350 Wins Club)

##  (a).  

W <- c(373,354,364,417,355,373,361,363,511)
L <- c(208,184,310,279,227,188,208,245,316)
Name <- c("Alexander","Clemens","Galvin","Johnson","Maddux","Mathewson",
           "Nichols","Spahn","Young")

##  (b).

Win.PCT <- 100*(W/(W+L))
Wins.350 <- data.frame(Name,W,L,Win.PCT)

##  (d).  

Wins.350 <- Wins.350[order(Wins.350$Win.PCT),]

##  4.  (Pichers in the 350 Wins Club, Continued)

##  (a).

SO <- c(2198,4672,1806,3509,3371,2502,1868,2583,2803)
BB <- c(951,1580,745,1363,999,844,1268,1434,1217)

##  (b)

SO.BB.Ratio <- SO/BB

##  (c)

SO.BB <- data.frame(Name, SO, BB, SO.BB.Ratio)

##  (d)

subset(SO.BB, SO.BB.Ratio >= 2.8)

SO.BB[SO.BB$SO.BB.Ratio>=2.8,]

##  (e)

SO.BB <- SO.BB[order(SO.BB$BB),]
SO.BB

##  5. (Pitcher/Strikeout/Walk Ratios)
##  Note:  My method here is different from the one outlined in the problem.  
##  I followed the same technique as section 2.9.2.  

##  (a)

Pitching <- read.csv("pitching.csv")

##  (b)

stats <- function(d){
    c.SO <- sum(d$SO, na.rm=TRUE)
    c.BB <- sum(d$BB, na.rm=TRUE)
    c.IPouts <- sum(d$IPouts,na.rm=TRUE)
    c.midYear <- median(d$yearID, na.rm=TRUE)
    data.frame(SO=c.SO, BB=c.BB, IPouts=c.IPouts, midYear=c.midYear)
}

library(plyr)

c.IPouts <- ddply(Pitching, .(playerID), summarize, 
                  c.IPouts=sum(IPouts,na.rm=TRUE))

##  (c)

Pitching.merge <- merge(Pitching,c.IPouts, by="playerID")

##  (d)

Pitching.10000 <- Pitching.merge[Pitching.merge$c.IPouts>=10000,]

Pitching.10000.stats <- ddply(Pitching.10000, .(playerID), stats)

Pitching.10000.stats <- Pitching.10000.stats[order(Pitching.10000.stats$midYear),]

x11()
with(Pitching.10000.stats, plot(midYear,SO/BB))
