

library(ffanalytics)

tools::package_dependencies(ffanalytics)

install.packages(c("reshape", "MASS", "psych", "Rglpk", "XML", "data.table"), dependencies=TRUE)


#gets projections for the given week
scrapetest <- runScrape(season = NULL, week = weekNumber, analysts = 99,  pos = "all",
                        fbgUser = NULL, fbgPwd, updatePlayers = TRUE)
