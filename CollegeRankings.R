#Sample: 
#  theGames = GetGames()
#  theRankings = Algorithm(1000, theGames)
#  For each URL, run: tourney1 = GetGames(URL), then allGames = rbind(tourney1, tourney2, etc.), then theRankings = Algorithm(1000, allGames)

require(data.table)

GetGames = function(theURL = "https://play.usaultimate.org/events/Santa-Barbara-Invitational-2023/schedule/Men/CollegeMen/")
{
  simple = 
    read_html(theURL)
  
  others = simple %>% html_nodes("span") %>% html_text()
  others = gsub("[\r\n\t]", "", others)
  games = others[outer(-(8:1), which(others == "Final"), FUN = "+")]
  games = gsub(' \\([0-9]+\\)', '', games)
  gamesTable = as.data.table(matrix(games, ncol = 8, byrow = T))
  
  bFormat = is.na(as.numeric(gamesTable$V7))
  
  tab1 = gamesTable[!bFormat, list(team1 = V3, team2 = V4, score1 = V5, score2 = V7)]
  
  tab2 = gamesTable[bFormat, list(team1 = V3, team2 = V7, score1 = V1, score2 = V5)]
  
  tab = rbind(tab1, tab2)
  setkey(tab, team1)
  
  tab[, score1 := as.numeric(score1)]
  tab[, score2 := as.numeric(score2)]
  
  return(tab)
}


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
  
  Games[,winSign:=ifelse(score1 > score2, 1, -1)]
  
  Rankings = data.table(teams = unique(Games[,c(team1, team2)]), score = 1000)
  
  for(i in 1:count)
  {
    setkey(Rankings, teams)
    
    Games[,pointsDiff:= ratingDiff(score1, score2), by = 1:nrow(Games)]
    
    setkey(Games, team1)
    team2 = Rankings[Games][, team2points := score + pointsDiff * winSign * -1] # points earned for team 2
    setkey(Games, team2)
    team1 = Rankings[Games][, team1points := score + pointsDiff * winSign]
    
    allScores = rbind(team2[, list(teams = team2, points = team2points, maxScore = pointsDiff == 600, won = winSign < 0)], 
                      team1[, list(teams = team1, points = team1points, maxScore = pointsDiff == 600, won = winSign > 0)])
    
    allScores$opponent = c(team2[,teams], team1[,teams])
    allScores$FinalScore = c(team2[,paste0(score1, " - ", score2)], team1[, paste0(score1, " - ", score2)])
    allScores = data.table(allScores, key = "teams")
    allScores = allScores[Rankings]
    
    #get rid of games where a team won by max amount but it still takes down their record
    allScores = allScores[!((won) & (maxScore) & (points < score))]
    
    #get rid of games where a team lost by max amount but it still helps their record
    allScores = allScores[!((!won) & (maxScore) & (points > score))]
    
    if(i == count)
    {
      teamResults = allScores[,effect:=(points - score)/.N,by=teams]
      teamResults = teamResults[,list(Team = teams, opponent, FinalScore, effect, maxScore, won)]
      print(split(teamResults, teamResults$Team))
      print(allScores[,sum(effect),keyby=teams])
    }
    
    Rankings0 = allScores[,list(score = mean(points)),keyby=teams]
    
    Rankings = Rankings0[Rankings][is.na(score), score := i.score][,list(teams, score)]
  }
  
  Rankings = Rankings[order(-score)]
  
  return(Rankings)
}






