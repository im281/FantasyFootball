
GetProjectedPoints <- function(weekNumber,pos){
  
  library(ffanalytics)
  library(data.table)
  
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
  scrapeData <- runScrape(season = 2017, week = weekNumber, analysts = 18,  pos,
                          fbgUser = NULL, fbgPwd, updatePlayers = TRUE)
  
  p <- unlist(attributes(scrapeData))
  
  #loop through all the positions here
  for (i in 1:6){
    if(p[i] == "QB"){
      espnQB <- scrapeData$QB@resultData
      #replace the playerId as character
      pid <- as.character(scrapeData$QB@resultData$playerId)
      scrapeData$QB@resultData$playerId <- pid
      espnQB <- scrapeData$QB@resultData
    }
    if(p[i] == "WR"){
      espnWR <- scrapeData$WR@resultData
      #replace the playerId as character
      pid <- as.character(scrapeData$WR@resultData$playerId)
      scrapeData$WR@resultData$playerId <- pid
      espnWR <- scrapeData$WR@resultData
    }
    if(p[i] == "DST"){
      espnDST <- scrapeData$DST@resultData
      #replace the playerId as character
      pid <- as.character(scrapeData$DST@resultData$playerId)
      scrapeData$DST@resultData$playerId <- pid
      espnDST <- scrapeData$DST@resultData
    }
    if(p[i] == "RB"){
      espnRB <- scrapeData$RB@resultData
      #replace the playerId as character
      pid <- as.character(scrapeData$RB@resultData$playerId)
      scrapeData$RB@resultData$playerId <- pid
      espnRB <- scrapeData$RB@resultData
    }
    if(p[i] == "TE"){
      espnTE <- scrapeData$TE@resultData
      #replace the playerId as character
      pid <- as.character(scrapeData$TE@resultData$playerId)
      scrapeData$TE@resultData$playerId <- pid
      espnTE <- scrapeData$TE@resultData
    }
    if(p[i] == "K"){
      espnK <- scrapeData$K@resultData
      #replace the playerId as character
      pid <- as.character(scrapeData$K@resultData$playerId)
      scrapeData$K@resultData$playerId <- pid
      espnK <- scrapeData$K@resultData
    }
  }
  
  espnF <- rbind(espnQB,espnWR,espnDST,espnRB,espnTE,espnK,fill = TRUE)
  
  
  
  # espn <- scrapeData[1]$WR@resultData
  # #replace the playerId as character
  # pid <- as.character(scrapeData[1]$WR@resultData$playerId)
  # scrapeData[1]$WR@resultData$playerId <- pid
  # espn <- scrapeData[1]$WR@resultData
  
  #merge NFL and ESPN data (Not sure why I did this!)
  data <- espnF#merge(espnF, nfl, all=FALSE, by = 'playerId')
  View(data)
  
  #get projected fantasy points given scoring rules
  pointsWR <- getMeltedData(scrapeData$WR)
  pointsQB <- getMeltedData(scrapeData$QB)
  pointsRB <- getMeltedData(scrapeData$RB)
  pointsTE <- getMeltedData(scrapeData$TE)
  pointsK <- getMeltedData(scrapeData$K)
  pointsDST <- getMeltedData(scrapeData$DST)
  
  points <- rbind(pointsWR,pointsQB,pointsRB,pointsTE,pointsK,pointsDST,fill = TRUE)
  
  
  projectedPoints <- calculatePoints(points,scoringRules = scoringRules)
  
  #replace the playerId as character
  prid <- as.character(projectedPoints$playerId)
  projectedPoints$playerId <- prid
  
  #merge NFL and ESPN data
  finalTable <- merge(data, projectedPoints, all=FALSE, by = 'playerId')
  
  return (finalTable)

}



