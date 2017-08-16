


library(ffanalytics)
library(data.table)


weekNumber = 1
pos = c("WR","QB","DST")


#fantsy points scoring rules
scoringRules <- list(
  QB = data.table::data.table(dataCol = c("passYds", "passTds", "passInt", "rushYds", "rushTds", "twoPts", "fumbles"),
                              multiplier = c(1/25, 4, -3, 1/10, 6, 2, -3 )),
  RB = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                              multiplier = c(1/10, 6, 0, 1/8, 6, 6, 2, -3)),
  WR = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                              multiplier = c(1/10, 6, 0, 1/8, 6, 6, 2, -3)),
  TE = data.table::data.table(dataCol = c("rushYds", "rushTds", "rec", "recYds", "recTds", "returnTds", "twoPts", "fumbles"),
                              multiplier = c(1/10, 6, 0, 1/8, 6, 6, 2, -3)),
  K = data.table::data.table(dataCol = c("xp", "fg0019", "fg2029", "fg3039", "fg4049", "fg50"),
                             multiplier = c(1,  3, 3, 3, 4, 5)),
  DST = data.table::data.table(dataCol = c("dstFumlRec", "dstInt", "dstSafety", "dstSack", "dstTd", "dstBlk"),
                               multiplier = c(2, 2, 2, 1, 6, 1.5)),
  DL = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                              multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
  LB =  data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                               multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
  DB = data.table::data.table(dataCol = c("idpSolo", "idpAst", "idpSack", "idpInt", "idpFumlForce", "idpFumlRec", "idpPD", "idpTd", "idpSafety"),
                              multiplier = c(1, 0.5, 2, 3, 3, 2, 1, 6, 2)),
  ptsBracket = data.table::data.table(threshold = c(0, 6, 20, 34, 99),
                                      points = c(10, 7, 4, 0, -4))
)


nfl <- nflPlayerData(season = 2017, weekNo = weekNumber, positions = pos)

#gets projections for the given week
scrapeData <- runScrape(season = NULL, week = weekNumber, analysts = 18,  pos,
                        fbgUser = NULL, fbgPwd, updatePlayers = TRUE)

p <- attributes(scrapeData)


for (i in length(p)-2){
  if(p$names[i] == "QB"){
    espn <- scrapeData$QB@resultData
    #replace the playerId as character
    pid <- as.character(scrapeData$QB@resultData$playerId)
    scrapeData$QB@resultData$playerId <- pid
    espn <- scrapeData$QB@resultData
  }
  if(p$names[i] == "WR"){
    espn <- scrapeData$WR@resultData
    #replace the playerId as character
    pid <- as.character(scrapeData$WR@resultData$playerId)
    scrapeData$WR@resultData$playerId <- pid
    espn <- scrapeData$WR@resultData
  }
  if(p$names[i] == "DST"){
    espn <- scrapeData$DST@resultData
    #replace the playerId as character
    pid <- as.character(scrapeData$DST@resultData$playerId)
    scrapeData$DST@resultData$playerId <- pid
    espn <- scrapeData$DST@resultData
  }
}

#merge NFL and ESPN data
data <- merge(espn, nfl, all=FALSE, by = 'playerId')
View(data)

#get projected fantasy points given scoring rules for all positions
  points <- getMeltedData(scrapeData$WR)




projectedPoints <- calculatePoints(points,scoringRules = scoringRules)

#replace the playerId as character
prid <- as.character(projectedPoints$playerId)
projectedPoints$playerId <- prid

#merge NFL and ESPN data
finalTable <- merge(data, projectedPoints, all=FALSE, by = 'playerId')
