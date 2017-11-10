rm(list=ls())
cat("\014")
#setwd('/Users/Deeksha/Dropbox (MIT)/PhD/sem-5/Analytics of OM/Fall2017/15.774 Analytics of OM - MBAN/Recitations/Recitation 06')
setwd('C:/Users/dgaru/Google Drive/MIT/Fall 2017/Analytics of Ops Mgmt/AOM-Project')

all_games = read.csv(file="all_games.csv",sep=";")
player_dat = read.csv(file="player_dat.csv")
names = read.csv(file="abbrv_names.csv",sep=";")

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
      return(toString(names[names$ï..Abbrev==team,2]))
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

spread <- function(team, season){
  seas = all_games[all_games$Season==season,]
  team = nabbr(team,'full')
  vis = seas[seas$VISITOR==team,]
  vis = data.frame('vs'=vis$HOME, 'spread'=vis$V.PTS-vis$H.PTS)
  home = seas[seas$HOME==team,]
  home = data.frame("vs"=home$VISITOR, "spread"=home$H.PTS-home$V.PTS)
  avg = c(vis$spread,home$spread)
  return(list("vis"=vis,"home"=home,"avg"=mean(avg)))
}

y=teams(2004)
x=roster('NYK', 2004)
z=spread('DET', 2004)