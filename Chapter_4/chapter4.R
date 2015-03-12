##  Chapter 4.  

##  4.2 The Teams Table in the Lahman's Database.  

teams <- read.csv("teams.csv")
tail(teams)

myteams <- teams[teams$yearID>2000 & teams$yearID<2012, c("teamID", "yearID",
                    "lgID","G","W","L","R","RA")]
tail(myteams)

myteams$RD <- with(myteams, R-RA)
myteams$Wpct <- with(myteams, W/(W+L))

x11()
plot(myteams$RD, myteams$Wpct, xlab="run differential", 
        ylab="winning percentage")

##  4.3 Linear Regression.  

linfit <- lm(Wpct~RD, data=myteams)
linfit

summary(linfit)

abline(a=coef(linfit)[1],b=coef(linfit)[2],lwd=2)

myteams$linWpct <- predict(linfit)
myteams$linResiduals <- residuals(linfit)

x11()
plot(myteams$RD, myteams$linResiduals, xlab="run differential", 
     ylab="residual")

abline(h=0, lty=3)
points(c(68,88), c(.0749,-.0733),pch=19)
text(68, .0749, "LAA '08", pos=4, cex=.8)
text(88, -.0733, "CLE '06", pos=4, cex=.8)

mean(myteams$linResiduals)
linRMSE <- sqrt(mean(myteams$linResiduals^2))

nrow(subset(myteams, abs(linResiduals)<linRMSE))/nrow(myteams)
nrow(subset(myteams, abs(linResiduals)<2*linRMSE))/nrow(myteams)

##  4.4 The Pythagorean Formula for Winning Percentage.  

myteams$pytWpct <- with(myteams, R^2/(R^2+RA^2))

myteams$pytResiduals <- myteams$Wpct-myteams$pytWpct
sqrt(mean(myteams$pytResiduals^2))

##  4.5 The Exponent in the Pythagorean Formula.  

myteams$logWratio <- log(myteams$W/myteams$L)
myteams$logRratio <- log(myteams$R/myteams$RA)
pytFit<- lm(logWratio ~ 0 + logRratio, data=myteams)
pytFit

##  4.6 Good and Bad Predictions by the Pythagorean Formula.  

gl2011 <- read.table("gl2011.txt",sep=",")
glheaders <- read.csv("game_log_header.csv")
names(gl2011) <- names(glheaders)
BOS2011 <- gl2011[gl2011$HomeTeam=="BOS" | gl2011$VisitingTeam=="BOS", 
            c("VisitingTeam","HomeTeam","VisitorRunsScored","HomeRunsScore")]
head(BOS2011)

BOS2011$ScoreDiff <- with(BOS2011, ifelse(HomeTeam == "BOS", 
                        HomeRunsScore-VisitorRunsScored, 
                        VisitorRunsScored-HomeRunsScore))
BOS2011$W <- BOS2011$ScoreDiff > 0
