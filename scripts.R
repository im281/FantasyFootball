library(ffanalytics)

nfl <- nflPlayerData(season = 2017, weekNo = 1, positions = 'WR')

#gets projections for the given week
s <- runScrape(season = NULL, week = 1, analysts = 3,  c("WR"),
               fbgUser = NULL, fbgPwd, updatePlayers = TRUE)


espn <- s[1]$WR@resultData
#replace the playerId as character
pid <- as.character(s[1]$WR@resultData$playerId)
s[1]$WR@resultData$playerId <- pid
espn <- s[1]$WR@resultData

#merge NFL and ESPN data
data <- merge(espn, nfl, all=FALSE, by = 'playerId')
View(data)


#gets projections for the given week
test <- runScrape(season = NULL, week = 1, analysts = NULL,  c("WR"),
                  fbgUser = NULL, fbgPwd, updatePlayers = TRUE)

