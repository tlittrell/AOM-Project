setwd("~/Dropbox (MIT)/AOM Project/Dave:Tim")

all_games = read.csv(file="all_games.csv",sep=";")
player_dat = read.csv(file="player_dat.csv")
names = read.csv(file="abbrv_names.csv",sep=";")
levels(names$Team) = c(levels(names$Team), "New Orleans/Oklahoma City Hornets", "Charlotte Bobcats")
names[names$Abbrev == "CHO",]$Team = "Charlotte Hornets"
names[names$Abbrev == "CHH",]$Team = "Charlotte Hornets"
names[names$Abbrev == "CHA",]$Team = "Charlotte Bobcats"
names[names$Abbrev == "NOK",]$Team = "New Orleans/Oklahoma City Hornets"

nabbr <- function(team, ret = 'abbrv'){
  if(ret=="abbrv"){
    if(nchar(team)>3){
      return(toString(names[names$Team==team,1]))
    }else{
      return(team)
    }
  }else{
    if(nchar(team)>3){
      return(team)
    }else{
      return(toString(names[names$Abbrev==team,2]))
    }
  }
}

roster <- function(team, season){
  if(nchar(team)>3){
    teamabbrv <- matrix(names[names$Team==team,1])
  }else{
    teamabbrv = team
  }

  players = player_dat[player_dat$Year==season,]
  return(players[players$Tm==teamabbrv[1],])
}

teams <- function(season){
  seas = all_games[all_games$Season==season,]
  
  return(matrix(unique(seas$VISITOR)))
}

team_spread <- function(team, season){
  team = nabbr(team,'full') 
  seas = all_games %>% filter(Season == season)
  vis = seas %>% filter(VISITOR == team) %>% mutate(spread = V.PTS - H.PTS)
  home = seas %>% filter(HOME == team) %>% mutate(spread = H.PTS - V.PTS)
  avg = c(vis$spread,home$spread)
  #return(list("vis"=vis,"home"=home,"avg"=mean(avg)))
  return(mean(avg))
}

library(reshape2)
temp = ClustData %>%
  group_by(Tm, Year, cluster) %>%
  summarise(Mins = sum(MP)) %>%
  mutate(MinPer = Mins/sum(Mins)) %>%
  dcast(Tm + Year ~ cluster, value.var = c("MinPer"), fill = 0)

temp2 = ClustData %>%
  group_by(Tm, Year) %>%
  summarise(WinScore = sum(MP * WS.48)/sum(MP))

regression = merge(temp, temp2) %>%
  filter(Year > 2000, Tm != "TOT") %>%
  mutate(Spread = mapply(team_spread, as.character(Tm), Year))

write.csv(regression, file = "RegressionData.csv")
