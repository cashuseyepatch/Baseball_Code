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
avgspeedFC$pitch_type

## Explores droplevels.   

test_avgspeedFC <- droplevels(avgspeedFC)
test_avgspeedFC$pitch_type
identical(avgspeedFC, test_avgspeedFC)

speedTest <- test_avgspeedFC[,"speed"] 
speedAvg <- avgspeedFC[,"speed"]

notEqual <- speedTest != speedAvg

notEqual

##

avgspeedFC <- droplevels(avgspeedFC)
avgspeedFC

x11()
dotplot(factor(season) ~ speed, groups=pitch_type,
        data=avgspeedFC,
        pch=c("C", "F"), cex=2)

##  6.2.7  

avgSpeed <- aggregate(speed ~ pitches + season, data=F4verl,
    FUN=mean)
X11()
xyplot(speed ~ pitches | factor(season), data=avgSpeed)
avgSpeedComb <- mean(F4verl$speed)

panel = function(...){
    panel.xyplot(...)
    panel.abline(v=100, lty="dotted")
    panel.abline(h=avgSpeedComb)
    panel.text(25,100, "avg. speed")
    panel.arrows(25,99.5,0, avgSpeedComb,
                 length = .1)
}

xyplot(speed ~ pitches | factor(season), 
       data=avgSpeed,
       panel = function(...){
           panel.xyplot(...)
           panel.abline(v=100, lty="dotted")
           panel.abline(h=avgSpeedComb)
           panel.text(25,100, "avg. speed")
           panel.arrows(25,99.5,0, avgSpeedComb,
                        length = .1)
       }
       )

##  6.2.8 Building a graph, step-by-step.  

NoHit <- subset(verlander, gamedate == "2011-05-07")

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type, 
       auto.key=TRUE,
       aspect="iso")

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type, 
       auto.key=TRUE,
       aspect="iso",
       xlim=c(-2.2, 2.2),
       ylim=c(0,5),
       xlab="Horizontal Location|n(ft. from middle of plate)",
       ylab="Vertical Location|n(ft. from ground)")

pitchnames <- c("change-up", "curveball", "4S-fastball", 
                "2S-fastball", "slider")
myKey <- list(space="right",
              boder=TRUE,
              cex.title=.8,
              title='pitch type',
              text=pitchnames,
              padding.text=4)

topKzone <- 3.5
botKzone <- 1.6
inKzone <- -.95
outKzone <- .95

xyplot(pz ~ px | batter_hand, data=NoHit, groups=pitch_type, 
       auto.key=myKey,
       aspect="iso",
       xlim=c(-2.2, 2.2),
       ylim=c(0,5),
       xlab="Horizontal Location|n(ft. from middle of plate)",
       ylab="Vertical Location|n(ft. from ground)",
       panel=function(...){
           panel.xyplot(...)
           panel.rect(inKzone, botKzone, outKzone, topKzone,
                      border="black",lty=3)
       }
)

##  6.3 The ggplot2 Package.  

##  6.3.1  Introduction.  

##  6.3.2  The cabrera dataset.  

sampleRows <- sample(1:nrow(cabrera), 20)
cabrera[sampleRows,]

##  6.3.3  The first layer.  

library(ggplot2)
p0 <- ggplot(data=cabrera, aes(x=hitx, y=hity))
p1 <- p0 + geom_point()
x11()
p1

## 6.3.4  Grouping factors.  

p0 <- ggplot(data=cabrera, aes(x=hitx, y=hity))
p1 <- p0 + geom_point(aes(color=hit_outcome))
p2 <- p1 + coord_equal()
p2

##  6.3.5  Multipanel conditioning (faceting).  

x11()
p3 <- p2 + facet_wrap(~season)
p3

##  6.3.6  Adding Elements.  

bases <- data.frame(x=c(0,90/sqrt(2), 0, -90/sqrt(2),0), 
                    y=c(0, 90/sqrt(2), 2 * 90/sqrt(2), 90/sqrt(2), 0))

p4 <- p3 + geom_path(aes(x=x, y=y), data=bases)
p5 <- p4 + 
    geom_segment(x=0, xend=300, y=0, yend=300) + 
    geom_segment(x=0, xend=-300, y=0, yend=300)
p5

##  6.3.7 Combining Information.  

cabreraStretch <- subset(cabrera, gamedate > "2012-08-31")
p0 <- ggplot(data=cabreraStretch, aes(x=hitx, y=hity))
p1 <- p0 + geom_point(aes(shape=hit_outcome, color=pitch_type, 
                         size=speed))
p2 <- p1 + coord_equal()
p3 <- p2 + geom_path(aes(x=x, y=y), data=bases)
p4 <- p3 + guides(col=guide_legend(ncol=2))
p5 <- p4 +
    geom_segment(x=0, xend=300, y=0, yend=300) + 
    geom_segment(x=0, xend=-300, y=0, yend=300)
p5

##  6.3.8 Adding a smooth line with error bands.  

v0 <- ggplot(F4verl, aes(x=pitches, y=speed))
v1 <- v0 + facet_wrap(~ season)
v2 <- v1 + geom_line(stat="hline", yintercept="mean", lty=3)
v3 <- v2 + geom_point(aes(pitches, speed), 
                      data=F4verl[sample(1:nrow(F4verl), 1000),])
v4 <- v3 + geom_smooth(col="black")
v5 <- v4 + geom_vline(aes(xintercept=100), col="black", lty=2)
v5

##  6.3.9 Dealing with cluttered charts.  

kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
    )
ggplot(F4verl, aes(px, pz)) +
    geom_point() +
    facet_wrap(~batter_hand) +
    coord_equal() +
    geom_path(aes(x, y), data=kZone, lwd=2, col="white")

ggplot(F4verl, aes(px, pz)) +
    stat_binhex() +
    facet_wrap(~batter_hand) +
    coord_equal() +
    geom_path(aes(x, y), data=kZone, lwd=2, col="white")

## 6.3.10  Adding a background image.  


