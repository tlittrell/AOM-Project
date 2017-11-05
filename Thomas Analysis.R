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
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, 
         -FT., -PTS, -X3P, -X2P, -TRB, -FT) %>%
  biScale(.,row.center = FALSE, row.scale = FALSE)

cor(df_cluster)

# check cluster fit
cluster_fit_curve = function(x){
  result = rep(0,x)
  for (i in 1:x){
    clus = kmeans(df_cluster,i)
    result[i] = clus$tot.withinss
  }
  ggplot() +
    aes(x = seq(1:x), y = result) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Number of clusters", y = "Within cluster SS")
}
cluster_fit_curve(10)

clusters = kmeans(df_cluster,4)
clusters
clusters$betweenss/clusters$totss

df2 = df2 %>%
  mutate(cluster = factor(clusters$cluster))

# Get representative players
df2 %>%
  filter(Year == 2016, MP > 2000) %>%
  select(Player, PTS, MP, cluster) %>%
  arrange_(~ desc(PTS)) %>%
  group_by_(~ cluster) %>%
  do(head(., n = 5))

# Summary statistics
df2 %>% group_by(cluster) %>%
  summarise(mean_pts = mean(PTS), mean_ast = mean(AST), mean_trb = mean(TRB))

# Plots
df2 %>%
  ggplot() +
  aes(x = cluster, y = PosNum) +
  geom_boxplot() +
  theme_bw()

# Box plots
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

 

