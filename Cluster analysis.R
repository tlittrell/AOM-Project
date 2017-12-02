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
scree_plot = function(x){
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
scree_plot(10)


# K-Means clustering
set.seed(144)
clusters = kmeans(df_cluster,4)
df2 = df2 %>%
  mutate(cluster = factor(clusters$cluster))


# Ward Hierarchical Clustering
# Taken from https://www.statmethods.net/advstats/cluster.html
# and https://www.r-bloggers.com/hierarchical-clustering-in-r-2/
# d = dist(df_cluster, method = "euclidean") # distance matrix
# fit = hclust(d, method="ward.D") 
# hierarchical_clusters = cutree(fit,4)
# df2 = df2 %>%
#   mutate(cluster = factor(hierarchical_clusters))


### ADD CLUSTER NAMES ###
### WARNING: MUST BE REDONE WHEN CLUSTERING IS RERUN ###
cluster_name = function(x){
  if (x == 1){
    return("Wing")
  }else if (x == 2){
    return("Center")
  }else if (x == 3){
    return("Guard")
  }else if (x==4){
    return("Forward")
  }
}


df2 = df2 %>%
  mutate(cluster_names = sapply(cluster,cluster_name))


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

#Taken from https://www.statmethods.net/advstats/cluster.html
library(cluster) 
clusplot(df_cluster, clusters$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

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

# over time
df2 %>%
  group_by(Year, cluster_names) %>%
  summarise(count = n()) %>%
  mutate(frac = count / sum(count) * 100) %>%
  ggplot() +
  aes(x = Year, y = frac, color = cluster_names) +
  geom_line() +
  theme_bw() +
  labs(y = "% of Players") +
  scale_color_discrete(name = "Cluster")

#########

library(rpart)
library(rpart.plot)
library(caret)
library(dplyr)
library(ggplot2)
library(caTools)
library(dummies)

### Create DF for CART
dfCART = df2 %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, 
         -FT., -PTS, -X3P, -X2P, -TRB, -FT, -Year)

### Creat training and testing split
set.seed(123)
spl = sample.split(dfCART$cluster, SplitRatio = 0.7)
dfCART_Train = subset(dfCART, spl==TRUE)
dfCART_Test = subset(dfCART, spl==FALSE)

### CART Model
CARTmod = rpart(cluster ~.,
                data = dfCART_Train, 
                method="class")


prp(CARTmod)
rpart.plot(CARTmod, extra = 100)

### CART Model Accuracy
PredictCART = predict(CARTmod, newdata=dfCART_Test, type="class")
table(dfCART_Test$cluster, PredictCART)
accuracy = sum(388,465,560,210)/nrow(dfCART_Test)

########## Alternate CART approach
dfCART_alt <- cbind(df2, dummy(df2$cluster, sep = "_"))
names(dfCART_alt)[34:37] <- c("cluster1","cluster2","cluster3","cluster4")

### Create DFs for CART
dfCART_1 = dfCART_alt %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, 
         -FT., -PTS, -X3P, -X2P, -TRB, -FT, -Year, -cluster, -cluster2, -cluster3, -cluster4)

dfCART_2 = dfCART_alt %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, 
         -FT., -PTS, -X3P, -X2P, -TRB, -FT, -Year, -cluster, -cluster1, -cluster3, -cluster4)

dfCART_3 = dfCART_alt %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, 
         -FT., -PTS, -X3P, -X2P, -TRB, -FT, -Year, -cluster, -cluster2, -cluster1, -cluster4)

dfCART_4 = dfCART_alt %>% 
  select(-Player, -MP, -Year, -X,-Rk,-Pos, -PosNum, -Age,-Tm,-G,-GS, -X3P., -FG., -FG, -FGA, 
         -FT., -PTS, -X3P, -X2P, -TRB, -FT, -Year, -cluster, -cluster2, -cluster3, -cluster1)

### Factor variables
dfCART_1$cluster1 <- as.factor(dfCART_1$cluster1)
dfCART_2$cluster2 <- as.factor(dfCART_2$cluster2)
dfCART_3$cluster3 <- as.factor(dfCART_3$cluster3)
dfCART_4$cluster4 <- as.factor(dfCART_4$cluster4)

### Training and Testing Split
set.seed(123)
spl = sample.split(dfCART_1$cluster1, SplitRatio = 0.7)
dfCART_Train1 = subset(dfCART_1, spl==TRUE)
dfCART_Test1 = subset(dfCART_1, spl==FALSE)

set.seed(123)
spl = sample.split(dfCART_2$cluster2, SplitRatio = 0.7)
dfCART_Train2 = subset(dfCART_2, spl==TRUE)
dfCART_Test2 = subset(dfCART_2, spl==FALSE)

set.seed(123)
spl = sample.split(dfCART_3$cluster3, SplitRatio = 0.7)
dfCART_Train3 = subset(dfCART_3, spl==TRUE)
dfCART_Test3 = subset(dfCART_3, spl==FALSE)

set.seed(123)
spl = sample.split(dfCART_4$cluster4, SplitRatio = 0.7)
dfCART_Train4 = subset(dfCART_4, spl==TRUE)
dfCART_Test4 = subset(dfCART_4, spl==FALSE)


### CART Models
CARTmod_1 = rpart(cluster1 ~.,
                  data = dfCART_Train1, 
                  method="class", 
                  minbucket=5, 
                  cp=0.02)

CARTmod_2 = rpart(cluster2 ~.,
                  data = dfCART_Train2, 
                  method="class", 
                  minbucket=5, 
                  cp=0.02)

CARTmod_3 = rpart(cluster3 ~.,
                  data = dfCART_Train3, 
                  method="class", 
                  minbucket=5, 
                  cp=0.02)

CARTmod_4 = rpart(cluster4 ~.,
                  data = dfCART_Train4, 
                  method="class", 
                  minbucket=5, 
                  cp=0.02)

### CART Model Accuracy
PredictCART1 = predict(CARTmod_1, newdata=dfCART_Test1, type="class")
PredictCART2 = predict(CARTmod_2, newdata=dfCART_Test2, type="class")
PredictCART3 = predict(CARTmod_3, newdata=dfCART_Test3, type="class")
PredictCART4 = predict(CARTmod_4, newdata=dfCART_Test4, type="class")

table(dfCART_Test1$cluster1, PredictCART1)
table(dfCART_Test2$cluster2, PredictCART2)
table(dfCART_Test3$cluster3, PredictCART3)
table(dfCART_Test4$cluster4, PredictCART4)

accuracy1 = sum(1492,385)/nrow(dfCART_Test1)
accuracy2 = sum(1453,413)/nrow(dfCART_Test2)
accuracy3 = sum(1241,555)/nrow(dfCART_Test3)
accuracy4 = sum(1584,255)/nrow(dfCART_Test4)

### CART Trees
prp(CARTmod_1)
rpart.plot(CARTmod_1, main="Cluster 1", extra = 100)

prp(CARTmod_2)
rpart.plot(CARTmod_2, main="Cluster 2", extra = 100)

prp(CARTmod_3)
rpart.plot(CARTmod_3, main="Cluster 3", extra = 100)

prp(CARTmod_4)
rpart.plot(CARTmod_4, main="Cluster 4", extra = 100)


write_csv(df2,"/Users/thomaslittrell/Dropbox (MIT)/AOM Project/Data/clustered_data.csv")

