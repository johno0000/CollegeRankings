#Sample: 
#  theGames = GetGames()
#  theRankings = Algorithm(1000, theGames)
#  For each URL, run: tourney1 = GetGames(URL), then allGames = rbind(tourney1, tourney2, etc.), then theRankings = Algorithm(1000, allGames)

require(data.table)
require(rvest)

# This function goes to the college regular season site and finds all the links to sanctioned tournaments
GetAllGames = function(theURL = "https://usaultimate.org/college/schedule/", sleep = 0)
{
  season = read_html(theURL)
  #this specifically pics out the links to the "results" page
  events = season %>% html_nodes(".results a") %>% html_attr('href')
  #the links go to the title page of the event, we want to go to the college men division of each event so we add this to each URL
  events = paste0(events, "schedule/Men/CollegeMen/")
  #because of how USAU lays out these links, some will be duplicated, so get rid of those to not double count games
  events[!duplicated(events)]
  
  #now run the function GetGamesFromTournament on each URL. If it doesnt work, keep the error but keep running
  lap = lapply(events, function(URL) try(GetGamesFromTournament(URL, sleep)))
  # all the ones that returned error (because they havent happened or theres no mens division at that event) wont be data.table type, so this will pick out only the relevant games.
  games = rbindlist(lap[sapply(lap, is.data.table)]) 
  return(games)
}

GetGamesFromTournament = function(theURL, sleep = 0)
{
  #Sometimes websites will block you from too many simultaneous requests, 
  Sys.sleep(sleep)
  simple = 
    read_html(theURL)
  
  #Some string substiution/regex stuff to get the individual scores. The key to getting all this to work is the "final" tag after every finished game
  others = simple %>% html_nodes("span") %>% html_text()
  others = gsub("[\r\n\t]", "", others)
  games = others[outer(-(8:1), which(others == "Final"), FUN = "+")]
  games = gsub(' \\([0-9]+\\)', '', games)
  gamesTable = as.data.table(matrix(games, ncol = 8, byrow = T))
  
  #boolean array that tells which rows are in which of the two possible formats for game scores
  bFormat = is.na(as.numeric(gamesTable$V7))
  
  #populate table of games
  tab1 = gamesTable[!bFormat, list(team1 = V3, team2 = V4, score1 = V5, score2 = V7)]
  
  tab2 = gamesTable[bFormat, list(team1 = V3, team2 = V7, score1 = V1, score2 = V5)]
  
  tab = rbind(tab1, tab2)
  setkey(tab, team1)
  
  tab[, score1 := as.numeric(score1)]
  tab[, score2 := as.numeric(score2)]
  
  #removes duplicates, but side effect of if any games are actually repeated to the exact same score we'll miss them
  tab = tab[!duplicated(tab)]
  
  print(paste0(theURL, " success"))
  return(tab)
}

#USAU algorithm
ratingDiff = function(score1, score2) 
{
  winScore = max(score1, score2)
  lossScore = min(score1, score2)
  
  r = lossScore / (winScore - 1) 
  arg = min(c(1, (1 - r) / 0.5))
  x = 125 + 475 * sin(arg * 0.4 * pi) / sin(0.4 * pi)
  return(x)
}

Algorithm = function(count, Games, IncludeForfeits = TRUE)
{
  #could also switch columns so winner is always in same column, either way works
  Games[, winSign := ifelse(score1 > score2, 1, -1)]
  
  #builds initiall rankings table of all teams with any games, assign 1000 points to each team to start
  Rankings = data.table(teams = unique(Games[,c(team1, team2)]), score = 1000)
  
  for(i in 1:count)
  {
    #setting the key of the Rankings table and the Games table allows to easily cross reference the two
    setkey(Rankings, teams)
    
    Games[, pointsDiff:= ratingDiff(score1, score2), by = 1:nrow(Games)]
    
    #Note that score here refers to each teams ranking score, not the game score
    setkey(Games, team1)
    team2 = Rankings[Games][, team2points := score + pointsDiff * winSign * -1] # points earned for team 2 due to their game against team 1
    setkey(Games, team2)
    team1 = Rankings[Games][, team1points := score + pointsDiff * winSign] # same but reversed
    
    allScores = rbind(team2[, list(teams = team2, points = team2points, maxScore = pointsDiff == 600, won = winSign < 0)], 
                      team1[, list(teams = team1, points = team1points, maxScore = pointsDiff == 600, won = winSign > 0)])
    
    allScores$opponent = c(team2[,teams], team1[,teams])
    #This line is just for purposes of printout, not necessary for everything to run. Not sure why I put it here and not later
    allScores$FinalScore = c(team2[,paste0(score1, " - ", score2)], team1[, paste0(score1, " - ", score2)])
    allScores = data.table(allScores, key = "teams")
    #This merges (by team) the info in the allScores and Rankings tables
    allScores = allScores[Rankings]
    
    #get rid of games where a team won by max amount but it still takes down their record
    allScores = allScores[!((won) & (maxScore) & (points < score))]
    
    #get rid of games where a team lost by max amount but it still helps their record
    allScores = allScores[!((!won) & (maxScore) & (points > score))]
    
    #print detailed info on the final run with analysis for each team
    if(i == count)
    {
      teamResults = allScores[,effect:=(points - score)/.N, by = teams]
      teamResults = teamResults[,list(Team = teams, opponent, FinalScore, effect, maxScore, won)]
      print(split(teamResults, teamResults$Team))
      print(allScores[,sum(effect),keyby=teams])
    }
    #Each team is now given a new ranking based on the average of all their games
    Rankings0 = allScores[,list(score = mean(points)),keyby=teams]
    
    #Actually cant remember why/if some of this was necessary. Maybe earlier in season there were NA scores
    Rankings = Rankings0[Rankings][is.na(score), score := i.score][,list(teams, score)]
  }
  
  Rankings = Rankings[order(-score)]
  
  return(Rankings)
}






