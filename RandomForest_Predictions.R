

###########################################
###########################################
########################################### Import Data tables provided by Kaggle website

## Set working directory where files are stored
setwd("C:/Users/David/Desktop/ncaa/2016 Data")


## Read files into R
teams <- read.csv("Teams.csv", header = TRUE, stringsAsFactors = FALSE)
regSeason <- read.csv("RegularSeasonCompactResults.csv", header = TRUE, stringsAsFactors = FALSE)
seasons <- read.csv("Seasons.csv", header = TRUE, stringsAsFactors = FALSE)
tourneyRes <- read.csv("TourneyCompactResults.csv", header = TRUE, stringsAsFactors = FALSE)
tourneyResDetail <- read.csv("TourneyDetailedResults.csv", header = TRUE, stringsAsFactors = FALSE)


tourneySeeds <- read.csv("TourneySeeds.csv", header = TRUE, stringsAsFactors = FALSE)
tourneySlots <- read.csv("TourneySlots.csv", header = TRUE, stringsAsFactors = FALSE)
regSeasonDetail <- read.csv("RegularSeasonDetailedResults.csv", header = TRUE, stringsAsFactors = FALSE)





###########################################
###########################################
########################################### Create a training dataset


## Create "match-up" column 
games <- ifelse(test = tourneyRes$Wteam < tourneyRes$Lteam,
                yes = paste(tourneyRes$Season, "_", tourneyRes$Wteam, "_", tourneyRes$Lteam, sep = ""),
                no = paste(tourneyRes$Season, "_", tourneyRes$Lteam, "_", tourneyRes$Wteam, sep = ""))

## Identify Team A
teamA <-  ifelse(test = tourneyRes$Wteam < tourneyRes$Lteam,
       yes = tourneyRes$Wteam,
       no = tourneyRes$Lteam)

## Identify Team B
teamB <-  ifelse(test = tourneyRes$Wteam < tourneyRes$Lteam,
		 yes = tourneyRes$Lteam,
		 no = tourneyRes$Wteam)


outcome <- ifelse(test = tourneyRes$Wteam < tourneyRes$Lteam,
                  yes = 1,
                  no = 0)

## Create initial training dataset
trainData <- data.frame(Matchup = games, Win = outcome, Season=tourneyRes$Season,TeamA = teamA, TeamB = teamB)



## Wins by teams during regular season
gamesWon <- as.data.frame(table(regSeason$Wteam, regSeason$Season), stringsAsFactors = FALSE)
gamesLost <- as.data.frame(table(regSeason$Lteam, regSeason$Season), stringsAsFactors = FALSE)

names(gamesWon) <- c("Team", "Season", "Wins")
names(gamesLost) <- c("Team", "Season", "Losses")
gamesWon$Season <- as.integer(gamesWon$Season)
gamesWon$Team <- as.integer(gamesWon$Team)

gamesLost$Season <- as.integer(gamesLost$Season)
gamesLost$Team <- as.integer(gamesLost$Team)



## Load packages to manipulate data
library(magrittr) 
library(dplyr)


## Note: I wanted to learn how to use the pipe operator (%>%) in R, 
## which is why I used it frequently below.


## Join games lost and games one into one table
tourneySeeds <- tourneySeeds  %>% inner_join(gamesWon, by=c("Team", "Season")) %>% inner_join(gamesLost, by=c("Team", "Season"))

## Identify Seed number
tourneySeeds <- tourneySeeds %>% mutate(SeedValue = as.numeric(gsub(pattern = "[A-Z]",ignore.case = TRUE, replacement = "",x = tourneySeeds$Seed)))

## Calculate regular season winning percentage
tourneySeeds <- tourneySeeds %>% mutate(TotalWinPct = round(Wins/(Wins+Losses), digits = 3))


## Identify Wins by team
wins <- cbind(Season = regSeasonDetail$Season, 
	      Team = regSeasonDetail$Wteam,
	      Opponent = regSeasonDetail$Lteam,
	      Daynum = regSeasonDetail$Daynum, 
	      Location = regSeasonDetail$Wloc,
	      Result = "W", 
	      TeamScore = regSeasonDetail$Wscore,
	      NumOt = regSeasonDetail$Numot,
	      TeamFgm = regSeasonDetail$Wfgm,
	      TeamFga = regSeasonDetail$Wfga,
	      TeamFgm3 = regSeasonDetail$Wfgm3,
	      TeamFga3 = regSeasonDetail$Wfga3,
	      TeamFtm = regSeasonDetail$Wftm,
	      TeamFta = regSeasonDetail$Wfta,
	      TeamOr = regSeasonDetail$Wor,
	      TeamDr = regSeasonDetail$Wdr,
	      TeamAst = regSeasonDetail$Wast,
	      TeamTo = regSeasonDetail$Wto,
	      TeamStl = regSeasonDetail$Wstl,
	      TeamBlk = regSeasonDetail$Wblk,
	      ## Opponent Statistics	
	      OppScore = regSeasonDetail$Lscore,
	      OppFgm = regSeasonDetail$Lfgm,
	      OppFga = regSeasonDetail$Lfga,
	      OppFgm3 = regSeasonDetail$Lfgm3,
	      OppFga3 = regSeasonDetail$Lfga3,
	      OppFtm = regSeasonDetail$Lftm,
	      OppFta = regSeasonDetail$Lfta,
	      OppOr = regSeasonDetail$Lor,
	      OppDr = regSeasonDetail$Ldr,
	      OppAst = regSeasonDetail$Last,
	      OppTo = regSeasonDetail$Lto,
	      OppStl = regSeasonDetail$Lstl,
	      OppBlk = regSeasonDetail$Lblk,	
	      ## Point Differential 
	      PointDiff = regSeasonDetail$Wscore -  regSeasonDetail$Lscore)


## Identify losses by team
losses <- cbind(Season = regSeasonDetail$Season, 
	      Team = regSeasonDetail$Lteam,
	      Opponent = regSeasonDetail$Wteam,
	      Daynum = regSeasonDetail$Daynum, 
	      Location = ifelse(test = regSeasonDetail$Wloc == "H", yes = "A", no = "H"),
	      Result = "L", 
	      TeamScore = regSeasonDetail$Lscore,
	      Numot = regSeasonDetail$Numot,
	      TeamFgm = regSeasonDetail$Lfgm,
	      TeamFga = regSeasonDetail$Lfga,
	      TeamFgm3 = regSeasonDetail$Lfgm3,
	      TeamFga3 = regSeasonDetail$Lfga3,
	      TeamFtm = regSeasonDetail$Lftm,
	      TeamFta = regSeasonDetail$Lfta,
	      TeamOr = regSeasonDetail$Lor,
	      TeamDr = regSeasonDetail$Ldr,
	      TeamAst = regSeasonDetail$Last,
	      TeamTo = regSeasonDetail$Lto,
	      TeamStl = regSeasonDetail$Lstl,
	      TeamBlk = regSeasonDetail$Lblk,
	      ## Opponent Statistics	
	      OppScore = regSeasonDetail$Wscore,
	      OppFgm = regSeasonDetail$Wfgm,
	      OppFga = regSeasonDetail$Wfga,
	      OppFgm3 = regSeasonDetail$Wfgm3,
	      OppFga3 = regSeasonDetail$Wfga3,
	      OppFtm = regSeasonDetail$Wftm,
	      OppFta = regSeasonDetail$Wfta,
	      OppOr = regSeasonDetail$Wor,
	      OppDr = regSeasonDetail$Wdr,
	      OppAst = regSeasonDetail$Wast,
	      OppTo = regSeasonDetail$Wto,
	      OppStl = regSeasonDetail$Wstl,
	      OppBlk = regSeasonDetail$Wblk,	
	      ## Point Differential 
	      PointDiff = regSeasonDetail$Lscore -  regSeasonDetail$Wscore)


## Union wins and losses into one dataframe
allGames <- as.data.frame(rbind(wins, losses),stringsAsFactors = FALSE)

## Create copy of table to save time later
allGamesCopy <- allGames

## Convert all columns of table to numeric 
allGames <- as.data.frame(apply(X = allGames,MARGIN = 2,FUN = function(y) as.numeric(y)))

## Replace location and result variables with character replacements (from copy)
allGames$Location = allGamesCopy$Location
allGames$Result = allGamesCopy$Result

## Remove copy of table from memory
rm(allGamesCopy) 

## Sort Column by season, team, and daynum
allGames <- allGames[with(allGames, order(Season, Team, Daynum)), ]



## Split dataframe into lists by season & team
## Each season-team combination is now in its own dataframe in a large list 
TeamSeasons <- split(x = allGames, f = list(allGames$Season, allGames$Team))
TeamSeasons <- lapply(X = TeamSeasons,FUN = transform, cumulativeWins = cumsum(Result=='W')) ## haven't used yet. 
TeamSeasons <- lapply(X = TeamSeasons,FUN = transform, cumulativeLosses = cumsum(Result=='L'))  ## haven't used yet



###########################################
###########################################
###########################################
## The next steps loop over seasons and teams in TeamSeasons list to create summary variables by season and team


## Identify the number of wins in the last six games for team each season
LastSix <- lapply(X = TeamSeasons,FUN = tail)
LastSix <- lapply(X = LastSix,FUN = function(y) sum(y$Result=="W"))
LastSix <- data.frame(WinsLastSix = unlist(LastSix, recursive = TRUE, use.names = TRUE))
LastSix$Season <- as.integer(as.character(substr(x = rownames(LastSix),start = 1, stop = 4)))
LastSix$Team <- as.integer(as.character(substr(x = rownames(LastSix), start = 6, stop = 9)))


## Average points for team each season
TeamPointsAvg <- lapply(X = TeamSeasons,FUN = function(y) mean(y$TeamScore))
TeamPointsAvg <- data.frame(TeamPointsAvg = unlist(TeamPointsAvg, recursive = TRUE, use.names = TRUE))
TeamPointsAvg$Season <- as.integer(as.character(substr(x = rownames(TeamPointsAvg),start = 1, stop = 4)))
TeamPointsAvg$Team <- as.integer(as.character(substr(x = rownames(TeamPointsAvg), start = 6, stop = 9)))



## Average points against team each season
OppPointsAvg <- lapply(X = TeamSeasons,FUN = function(y) mean(y$OppScore))
OppPointsAvg <- data.frame(OppPointsAvg = unlist(OppPointsAvg, recursive = TRUE, use.names = TRUE))
OppPointsAvg$Season <- as.integer(as.character(substr(x = rownames(OppPointsAvg),start = 1, stop = 4)))
OppPointsAvg$Team <- as.integer(as.character(substr(x = rownames(OppPointsAvg), start = 6, stop = 9)))


## Average points for team each season
TeamAvgFgPercentage <- lapply(X = TeamSeasons,FUN = function(y) mean((y$TeamFgm/y$TeamFga)))
TeamAvgFgPercentage <- data.frame(TeamAvgFgPercentage = unlist(TeamAvgFgPercentage, recursive = TRUE, use.names = TRUE))
TeamAvgFgPercentage$Season <- as.integer(as.character(substr(x = rownames(TeamAvgFgPercentage),start = 1, stop = 4)))
TeamAvgFgPercentage$Team <- as.integer(as.character(substr(x = rownames(TeamAvgFgPercentage), start = 6, stop = 9)))


## Average field goal percentage against team each season
OppAvgFgPercentage <- lapply(X = TeamSeasons,FUN = function(y) mean((y$OppFgm/y$OppFga)))
OppAvgFgPercentage <- data.frame(OppAvgFgPercentage = unlist(OppAvgFgPercentage, recursive = TRUE, use.names = TRUE))
OppAvgFgPercentage$Season <- as.integer(as.character(substr(x = rownames(OppAvgFgPercentage),start = 1, stop = 4)))
OppAvgFgPercentage$Team <- as.integer(as.character(substr(x = rownames(OppAvgFgPercentage), start = 6, stop = 9)))


## Average turnover differential by team each season
TeamAvgTODiff <- lapply(X = TeamSeasons,FUN = function(y) mean(y$TeamTo-y$OppTo))
TeamAvgTODiff <- data.frame(TeamAvgTODiff = unlist(TeamAvgTODiff, recursive = TRUE, use.names = TRUE))
TeamAvgTODiff$Season <- as.integer(as.character(substr(x = rownames(TeamAvgTODiff),start = 1, stop = 4)))
TeamAvgTODiff$Team <- as.integer(as.character(substr(x = rownames(TeamAvgTODiff), start = 6, stop = 9)))


## Count of games for team each season
GamesCount <- lapply(X = TeamSeasons,FUN = function(y) length(y$Team))
GamesCount <- data.frame(GamesCount = unlist(GamesCount, recursive = TRUE, use.names = TRUE))
GamesCount$Season <- as.integer(as.character(substr(x = rownames(GamesCount),start = 1, stop = 4)))
GamesCount$Team <- as.integer(as.character(substr(x = rownames(GamesCount), start = 6, stop = 9)))


## Count of close wins for team each season (games within 10 points)
CloseGamesCount <- lapply(X = TeamSeasons,FUN = function(y) filter(y, Result == "W",PointDiff < 10) %>% summarise(length(PointDiff)))
CloseGamesCount <- data.frame(CloseGamesCount = unlist(CloseGamesCount, recursive = TRUE, use.names = TRUE))
CloseGamesCount$Season <- as.integer(as.character(substr(x = rownames(CloseGamesCount),start = 1, stop = 4)))
CloseGamesCount$Team <- as.integer(as.character(substr(x = rownames(CloseGamesCount), start = 6, stop = 9)))


## Count of close wins for team each season (games within 10 points)
## Average turnover differential by team each season
FreeThrowsPercent <- lapply(X = TeamSeasons,FUN = function(y) mean(y$TeamFtm/y$TeamFta,na.rm = TRUE))
FreeThrowsPercent <- data.frame(FreeThrowsPercent = unlist(FreeThrowsPercent, recursive = TRUE, use.names = TRUE))
FreeThrowsPercent$Season <- as.integer(as.character(substr(x = rownames(FreeThrowsPercent),start = 1, stop = 4)))
FreeThrowsPercent$Team <- as.integer(as.character(substr(x = rownames(FreeThrowsPercent), start = 6, stop = 9)))

OffensiveReboundsAvg <- lapply(X = TeamSeasons,FUN = function(y) mean(y$TeamOr))
OffensiveReboundsAvg <- data.frame(OffensiveReboundsAvg = unlist(OffensiveReboundsAvg, recursive = TRUE, use.names = TRUE))
OffensiveReboundsAvg$Season <- as.integer(as.character(substr(x = rownames(OffensiveReboundsAvg),start = 1, stop = 4)))
OffensiveReboundsAvg$Team <- as.integer(as.character(substr(x = rownames(OffensiveReboundsAvg), start = 6, stop = 9)))




## Join statistics to tourney seeds table
AllTeamStats <- tourneySeeds %>% 
	inner_join(teams,by=c("Team"="Team_Id")) %>%		
	inner_join(LastSix, by=c("Team", "Season")) %>% 
	inner_join(TeamPointsAvg, by=c("Team", "Season")) %>% 
	inner_join(OppPointsAvg, by=c("Team", "Season")) %>%
	inner_join(TeamAvgFgPercentage, by=c("Team", "Season")) %>%
	inner_join(OppAvgFgPercentage, by=c("Team", "Season")) %>%
	inner_join(TeamAvgTODiff, by=c("Team", "Season")) %>% 
	inner_join(GamesCount, by=c("Team", "Season")) %>% 
	inner_join(CloseGamesCount, by=c("Team", "Season")) %>%
	inner_join(FreeThrowsPercent, by=c("Team", "Season")) %>%
	inner_join(OffensiveReboundsAvg, by=c("Team", "Season")) 




AllTeamStats$CloseGamesPercent <- AllTeamStats$CloseGamesCount/AllTeamStats$GamesCount

################################################################ 
################################################################ 
################################################################ 
## Remove unneccessary Data
rm(tourneySeeds, teams,regSeason, seasons, tourneyRes, tourneyResDetail, tourneySlots, 
   regSeasonDetail, games, teamA, teamB, outcome, gamesWon, gamesLost, wins, 
   losses, allGames, LastSix, TeamPointsAvg, OppPointsAvg, TeamAvgFgPercentage, 
   OppAvgFgPercentage, TeamAvgTODiff, GamesCount, CloseGamesCount, FreeThrowsPercent, OffensiveReboundsAvg)

################################################################ 
################################################################ 
################################################################ 

## Create a table with TeamA names
AllTeamStatsA <- AllTeamStats
names(AllTeamStatsA) <- c("Season", "SeedA", "Team", "WinsA", "LossesA", "SeedValueA", 
			  "TotalWinPctA", "Team_NameA", "WinsLastSixA", "TeamPointsAvgA", 
			  "OppPointsAvgA", "TeamAvgFgPercentageA","OppAvgFgPercentageA", 
			  "TeamAvgTODiffA","GamesCountA", "CloseGamesCountA", 
			  "CloseGamesPercentA","FreeThrowsPercentA", "OffensiveReboundsAvgA")


## Create a table with TeamB names
AllTeamStatsB <- AllTeamStats
names(AllTeamStatsB) <- c("Season", "SeedB", "Team", "WinsB", "LossesB", "SeedValueB", 
			  "TotalWinPctB", "Team_NameB", "WinsLastSixB", "TeamPointsAvgB", 
			  "OppPointsAvgB", "TeamAvgFgPercentageB", "OppAvgFgPercentageB", 
			  "TeamAvgTODiffB", "GamesCountB", "CloseGamesCountB", 
			  "CloseGamesPercentB","FreeThrowsPercentB", "OffensiveReboundsAvgB")










## Join trainData to table with TeamA data and table with TeamB data
trainDataFull <- trainData %>% inner_join(AllTeamStatsA,by = c("TeamA" = "Team", "Season"))  %>% inner_join(AllTeamStatsB,by = c("TeamB" = "Team", "Season"))

trainDataFull$Win <- as.factor(trainDataFull$Win)

#tourneySeeds <- tourneySeeds[,c(1:3,8,4:7,9:16)]





#########################################
#########################################
######################################### Create first model for predictions

trainDataSubset <- trainDataFull[trainDataFull$Season <= 2011,]
testDataSubset <- trainDataFull[trainDataFull$Season >2011,]

## First attempt: create a random forest decision tree model to predict game outcomes
library(randomForest)

train_randomForest <- randomForest(formula = Win ~ 
				   	TotalWinPctA + TotalWinPctB + 
				   	#SeedValueA + SeedValueB + 
				   	WinsLastSixA + WinsLastSixB +
				   	TeamPointsAvgA + TeamPointsAvgB + 
				   	OppPointsAvgA + OppPointsAvgB + 
				   	TeamAvgFgPercentageA + TeamAvgFgPercentageB +
				   	OppAvgFgPercentageA + OppAvgFgPercentageB + 
				   	TeamAvgTODiffA + TeamAvgTODiffB + 
				   	CloseGamesPercentA + CloseGamesPercentB + 
				   	FreeThrowsPercentA + FreeThrowsPercentB +
					OffensiveReboundsAvgA + OffensiveReboundsAvgA,
				   data = trainDataSubset, ntree = 500)

## View summary of random forest model
train_randomForest


## Use model to predict on test data
testDataSubset$Predprob <- predict(object = train_randomForest,newdata = testDataSubset,type = "prob")[,2]
testDataSubset$Predwin <- predict(object = train_randomForest,newdata = testDataSubset,type = "response")



view_testDataSubset <- testDataSubset[,c("Matchup","Team_NameA","Team_NameB","SeedValueA","SeedValueB","Win","Predwin","Predprob")]
View(view_testDataSubset)



## View Accuracy Table
table(actual = view_testDataSubset$Win, pred = view_testDataSubset$Predwin)


## Create submission file
sampleSubmission <- read.csv("SampleSubmission.csv", header = TRUE, stringsAsFactors = FALSE)

library(stringr)
sampleSubmission$Season <- as.integer(substring(text = sampleSubmission$Id,first = 0,last = 4))
sampleSubmission$TeamA <- as.integer(substring(text = sampleSubmission$Id,first = 6,last = 9))
sampleSubmission$TeamB <- as.integer(substring(text = sampleSubmission$Id,first = 11,last = 14))


sampleSubmission <- sampleSubmission %>% 
	inner_join(AllTeamStatsA,by = c("TeamA" = "Team", "Season"))  %>% 
	inner_join(AllTeamStatsB,by = c("TeamB" = "Team", "Season")) 


## Add predicted values to submission file
sampleSubmission$Pred <- predict(train_randomForest,newdata = sampleSubmission,type = "prob")[,2]

Submission <- data.frame(Id = sampleSubmission$Id, 
			 PRed = sampleSubmission$Pred,
			 seeda = sampleSubmission$SeedValueA,
			 seedb = sampleSubmission$SeedValueB)

write.csv(x = Submission,file = "Submission1.csv")


##############################################################
##############################################################


###### Other variables to consider
## NCAA conference
## Wins against top 25 teams
## conference champion
## stength of victory
## strength of schedule
## avg game statistics
## avg game statistics last 6 games
## past season playoff success (legacy)
# Away Wins Winning Percentage
# Wins by margin less than 2
# Losses by margin less than 2
# Wins by margin greater than 7
# Losses by margin greater than 7
# Win Percentage in last 4 weeks
# Win Percentage against playoff teams
# Wins in Tournament
