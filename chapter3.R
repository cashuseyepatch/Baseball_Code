##  Ch. 3.  

##  3.2 Factor Variable.  

##  3.2.1 A Bar Graph.  

hof <- read.csv("hofbatting.csv")

hof$MidCareer <- with(hof, (From+To)/2)
hof$Era <- cut(hof$MidCareer, breaks=c(1800,1900,1919,1941,1960,1976,1993,2050)
               , labels=c("19th Century","Dead Ball", "Lively Ball", 
                          "Integration","Expansion","Free Agency","Long Ball"))

T.Era <- table(hof$Era)
T.Era
x11()
barplot(T.Era)

##  3.2.2 Add axes labels and a title.  

x11()
barplot(table(hof$Era), xlab="Era", ylab="Frequency",
        main="Era of the Nonpitching Hall of Famers")

##  3.2.3  Other graphs of a factor.  

plot(table(hof$Era))

pie(table(hof$Era))

##  3.3 Saving Graphs.  

png("bargraph.png")
barplot(table(hof$Era), xlab="Era", ylab="Frequency",
        main="Era of the Nonpitching Hall of Famers")
dev.off()
RStudioGD
    2

?png

pdf("graphs.pdf")
barplot(table(hof$Era))
plot(table(hof$Era))
dev.off()
RStudioGD
    2

##  3.4 Dot plots.  

T.Era <- table(hof$Era)
x11()
dotchart(as.numeric(T.Era),labels=names(T.Era),xlab="Frequency")

hof.500 <- hof[hof$HR>=500,]
hof.500 <- hof.500[order(hof.500$OPS),]
dotchart(hof.500$OPS,labels=hof.500$X,xlab="OPS")

##  3.5 Numeric Variable: Stripchar and Histogram.  

x11(width=7,height=3.5)
stripchart(hof$MidCareer, method="jitter",pch=1,xlab="Mid Career")

x11()
hist(hof$MidCareer, xlab="Mid Career", main="")

hist(hof$MidCareer,xlab="Mid Career", main="", breaks=seq(1880,2000,by=20))

##  3.6 Two Numeric Variables.  

x11()
with(hof, plot(MidCareer,OPS))
with(hof, lines(lowess(MidCareer,OPS,f=.3)))
with(hof,identify(MidCareer,OPS,X,n=4))

##  Bulding a graph, step-by-step.  

x11()
with(hof, plot(OBP, SLG))

with(hof, plot(OBP, SLG, xlim=c(0.25,0.50), ylim=c(0.28,0.75), pch=19, 
               xlab="On-Base Percentage", ylab="Slugging Percentage"))

curve(.7-x, add=TRUE)
curve(.8-x, add=TRUE)
curve(.9-x, add=TRUE)
curve(1.0-x, add=TRUE)

text(.27, .42, "OPS = .7")
text(.27, .52, "OPS = .8")
text(.27, .62, "OPS = .9")
text(.27, .72, "OPS = 1.0")

with(hof, identify(OBP, SLG, X, n=6))

##  3.7 A Numeric Variable and a Factor Variable.  

hof$HR.Rate <- with(hof, HR/AB)

##  3.7.1 Parallel stripcharts.  

x11()
stripchart(HR.Rate ~ Era, data=hof)

par(plt=c(.2, .94, .145, .883))
stripchart(HR.Rate ~ Era, data=hof,method="jitter",pch=1,las=2)

##  3.7.2 Parallel Boxplots.  

par(plt=c(.2, .94,.145,.883))
boxplot(HR.Rate ~ Era, data=hof, las=2, horizontal=TRUE, xlab="HR Rate")

##  3.8  Comparing Ruth, Aaron, Bonds, and A-Rod.  

##  3.8.1 Getting the data.  

master <- read.csv("Master.csv")

getinfo <- function(firstname, lastname){
    playerline <- master[master$nameFirst==firstname & 
                             master$nameLast==lastname,]
    name.code <- as.character(playerline$playerID)
    birthyear <- playerline$birthYear
    birthmonth <- playerline$birthMonth
    birthday <- playerline$birthDay
    byear <- ifelse(birthmonth <= 6, birthyear, birthyear+1)
    list(name.code=name.code, byear=byear)
}

ruth.info <- getinfo("Babe","Ruth")
aaron.info <- getinfo("Hank","Aaron")
bonds.info <- getinfo("Barry","Bonds")
arod.info <- getinfo("Alex","Rodriguez")
ruth.info

##  3.8.2 Creating the player data frames.  

batting <- read.csv("Batting.csv")

ruth.data <- batting[batting$playerID==ruth.info$name.code,]
ruth.data$Age <- ruth.data$yearID-ruth.info$byear

aaron.data <- batting[batting$playerID==aaron.info$name.code,]
aaron.data$Age <- aaron.data$yearID-aaron.info$byear

bonds.data <- batting[batting$playerID==bonds.info$name.code,]
bonds.data$Age <- bonds.data$yearID-bonds.info$byear

arod.data <- batting[batting$playerID==arod.info$name.code,]
arod.data$Age <- arod.data$yearID-arod.info$byear

##  3.8.3 Consturcting the graph. 

x11()

with(ruth.data, plot(Age,cumsum(HR),type="l",lty=3,lwd=2,xlab="Age",
                     ylab="Career Home Runs",xlim=c(18,45),ylim=c(0,800)))

with(aaron.data, lines(Age,cumsum(HR),lty=2,lwd=2))
with(bonds.data, lines(Age,cumsum(HR),lty=1,lwd=2))
with(arod.data, lines(Age,cumsum(HR),lty=4,lwd=2))
legend(20,700,legend=c("Bonds","Aaron","Ruth","Arod"),lty=1:4,lwd=2)

##  3.9 The 1998 Home Run Race.  

##  3.9.1  Getting the data.  

data1998 <- read.csv("all1998.csv", header=FALSE)
fields <- read.csv("fields.csv")
names(data1998) <- fields[,"Header"]

retro.ids <- read.csv("retrosheetIDs.csv")

sosa.id <- as.character(retro.ids[retro.ids$FIRST=="Sammy" & 
                                      retro.ids$LAST=="Sosa", "ID"])
mac.id <- as.character(retro.ids[retro.ids$FIRST=="Mark" & 
                                     retro.ids$LAST=="McGwire", "ID"])

sosa.data <- data1998[data1998$BAT_ID == sosa.id,]
mac.data <- data1998[data1998$BAT_ID==mac.id,]

##  3.9.2 Extracting the variables.  

createdata <- function(d){
    d$Date <- as.Date(substr(d$GAME_ID,4,11), format="%Y%m%d")
    d <- d[order(d$Date),]
    d$HR <- ifelse(d$EVENT_CD==23,1,0)
    d$cumsumHR <- cumsum(d$HR)
    d[,c("Date","cumsumHR")]
}

mac.hr <- createdata(mac.data)
sosa.hr <- createdata(sosa.data)
head(sosa.hr)

##  3.9.3 Constructing the graph.  

x11()
plot(mac.hr, type="l", lwd=2, ylab="Home Runs in the Season")
lines(sosa.hr,lwd=2,col="grey")
abline(h=62,lty=3)
text(10440,65,"62")
legend(10440,20,legend=c("McGwire (70)", "Sosa (66)"), lwd=2, 
       col=c("black","grey"))