library(data.table)
library(ffanalytics)

#get projection data
projections <- GetProjectedPoints(1,c("WR","QB","DST","TE","RB","K","DST"))
def <- projections[which(projections$position.x == "DST")]

#get fanduel data
fd <- data.table(read.csv("FanDuel-NFL-2017-09-10-20431-players-list.csv"))
fd <- fd[which(fd$Injury.Indicator == "")]

#replace column names
colnames(projections)[16] <- 'PlayerId'
colnames(def)[16] <- 'PlayerId'
colnames(def)[2] <- 'Position'
def$Position <- def$position.y
colnames(fd)[5] <- 'PlayerId'

#set primary keys to join tables
setkey(fd,PlayerId)
setkey(projections,PlayerId)
setkey(def,PlayerId)

# perform the join, eliminating not matched rows from Right
Result <- fd[projections, nomatch=0]

setkey(fd,Last.Name)
setkey(def,PlayerId)
test <- fd[def,nomatch = 0]

#add the defense data
alldata <- rbind(Result,test,fill = TRUE)

#run optimizer to get lineup
result <- RunOptimizerOnProjections(alldata)




