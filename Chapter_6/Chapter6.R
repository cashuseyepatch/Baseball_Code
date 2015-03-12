##  Chapter 6 Advanced Graphics.  

##  6.2.1 Introduction.  

load("balls_strikes_count.Rdata")

##  6.2 The Lattice Package.  

library(lattice)

##  6.2.2 The verlander dataset.  

sampleRows <- sample(1:nrow(verlander), 20)
verlander[sampleRows,]

##  6.2.3 Basic Plotting with Lattice.  

x11()
histogram(~speed, data=verlander)

x11()
densityplot(~ speed, data=verlander, plot.points=FALSE)

##  6.2.4 Multipanel conditioning.  

x11()
densityplot(~speed | pitch_type, data=verlander, 
    layout=c(1, 5), plot.points = FALSE)

##  6.2.5 Superposing group elements.  

x11()
densityplot(~ speed, data=verlander, groups=pitch_type,
    plot.points=FALSE, auto.key=TRUE)

##  6.2.6 Scatterplots and dot plots.  

F4verl <- subset(verlander, pitch_type == "FF")
F4verl$gameDay <- as.integer(format(F4verl$gamedate, format="%j"))
dailySpeed <- aggregate(speed ~ gameDay + season, data=F4verl,
    FUN=mean)

x11()
xyplot(speed ~ gameDay | factor(season),
        data=dailySpeed,
        xlab="day of the year",
        ylab="pitch speed (mph)")

speedFC <- subset(verlander, pitch_type %in% c("FF", "CH"))

## Sheds light on the %in% operator.  

test <- subset(verlander, (pitch_type == "FF") | (pitch_type == "CH"))
identical(speedFC, test)

## saves writing stuff.  

avgspeedFC <- aggregate(speed~ pitch_type + season, 
    data=speedFC, FUN=mean)
avgspeedFC

## Explores droplevels.   

test_avgspeedFC <- droplevels(avgspeedFC)
identical(avgspeedFC, test_avgspeedFC)

speedTest <- test_avgspeedFC[,"speed"] 
speedAvg <- avgspeedFC[,"speed"]

notEqual <- speedTest != speedAvg

notEqual

##

avgspeedFC <- droplevels(avgspeedFC)
avgspeedFC

