library(tidyverse)
library(softImpute)
setwd("~/Dropbox (Personal)/MIT/Fall 2017/Analytics of Operations Management/Project/AOM-Project")

# Read in and clean data
convert_pos = function(x){
  if (x == 'C'){
    return(5)
  } else if (x == 'C-PF'){
    return(4.75)
  } else if (x == 'C-SF'){
    return(4)
  } else if (x == 'PF'){
    return(4)
  } else if (x == 'PF-C'){
    return(4.25)
  } else if (x == 'PF-SF'){
    return(3.75)
  } else if (x == 'PG'){
    return(1)
  } else if (x == 'PG-SF'){
    return(2)
  } else if (x == 'PG-SG'){
    return(1.25)
  } else if (x == 'SF'){
    return(3)
  } else if (x == 'SF-PF'){
    return(3.25)
  } else if (x == 'SF-SG'){
    return(2.75)
  } else if (x == 'SG'){
    return(2)
  } else if (x == 'SG-PF'){
    return(3)
  } else if (x == 'SG-PG'){
    return(1.75)
  } else if (x == 'SG-SF'){
    return(2.25)
  }
}
df = read.csv("Combined Data.csv")
df2 = df %>%
  mutate(PosNum = sapply(df$Pos, convert_pos)) %>%
  filter(MP >= 500) 

# Run clusters and append to the data frame
df_cluster = df2 %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, -FT., -PTS) %>%
  biScale()

clusters = kmeans(df_cluster,5)
clusters

df2 = df2 %>%
  mutate(cluster = factor(clusters$cluster))


# Plots
df2 %>%
  ggplot() +
  aes(x = cluster, y = PosNum) +
  geom_boxplot() +
  theme_bw()

df2 %>%
  ggplot() + 
  aes(x = X3PA, y = X2PA, color = cluster) + 
  geom_point() +
  theme_bw()
  
df2 %>% 
  ggplot() + 
  aes(x = cluster, y = TRB) + 
  geom_boxplot() +
  theme_bw()

df2 %>%
  ggplot() +
  aes(x = cluster, y = AST) +
  geom_boxplot() +
  theme_bw()

df2 %>%
  ggplot() +
  aes(x = cluster, y = STL) +
  geom_boxplot() +
  theme_bw()

df2 %>%
  ggplot() +
  aes(x = cluster, y = BLK) +
  geom_boxplot() +
  theme_bw()

df2 %>%
  ggplot() +
  aes(x = cluster, y = TOV) +
  geom_boxplot() +
  theme_bw()

df2 %>%
  ggplot() +
  aes(x = cluster, y = PF) +
  geom_boxplot() +
  theme_bw()

 

