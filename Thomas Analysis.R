library(tidyverse)
setwd("~/Dropbox (Personal)/MIT/Fall 2017/Analytics of Operations Management/Project/AOM-Project")

df = read.csv("Combined Data.csv")
df2 = df %>%
  filter(MP >= 500) %>%
  select(-X,-Rk,-Pos,-Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, -FT., -TRB, -PTS) 
