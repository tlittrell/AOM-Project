library(tidyverse)
library(softImpute)
setwd("~/Dropbox (Personal)/MIT/Fall 2017/Analytics of Operations Management/Project/AOM-Project")

# Read in and clean data
df = read.csv("Combined Data.csv")
df2 = df %>%
  filter(MP >= 500) 


# Run clusters and append to the data frame
df_cluster = df2 %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos,-Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, -FT., -PTS) %>%
  biScale()

clusters = kmeans(df_cluster,5)
clusters

df2 = df2 %>%
  mutate(cluster = factor(clusters$cluster))


# Plots
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

a = df2 %>%
  group_by(cluster) %>%
  count(Pos) +
  theme_bw()
 
