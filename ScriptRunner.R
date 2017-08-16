library(data.table)
library(ffanalytics)

projections <- GetProjectedPoints(1,c("WR","QB","DST","TE","RB","K","DST"))

fd <- data.table(read.csv("week1fd.csv"))
#fd <- fd[which(fd$Injury.Indicator == "")]

colnames(projections)[16] <- 'PlayerId'
colnames(fd)[5] <- 'PlayerId'

setkey(fd,PlayerId)
setkey(projections,PlayerId)

# perform the join, eliminating not matched rows from Right
Result <- fd[projections]#, nomatch=0]

otable <- merge(fd,projections,by="PlayerId")
otable <- merge(fd, projections, by = 'PlayerId')

test <- RunOptimizerOnProjections(Result)




