library(readxl)
library(tidyverse)
library(stringr)
require(softImpute)
#install.packages("mclust")
library(mclust)
library(gridExtra)
#install.packages("reshape")
library(reshape)

setwd("~/Dropbox (MIT)/AOM Project/Data/All Data")

allData <- read.csv("Combined data.csv")

omit <- c("X", "Rk", "Player", "Pos", "Age", "Tm", "G", "GS", "MP", "Year")

dat <- allData %>% 
  filter(MP >= 300) %>%
  select(-one_of(omit))


Y = dat
Ybi <- biScale(Y,col.scale=FALSE,row.scale=FALSE,trace=TRUE)
res=softImpute(Ybi,rank.max=5,trace=TRUE,type="svd",lambda= 1)
Yhat_c = complete(Y,res,unscale=TRUE)

X <- data.matrix(scale(Yhat_c))
colMeans(X)
apply(X, 2, sd)

kclusts = 5
kmeanobj = kmeans(X, kclusts)

Ynew <- Yhat_c %>% 
  mutate(Cluster = kmeanobj$cluster)

Ynew %>%
  select(Cluster, colnames(Ynew)[1:7]) %>%
  melt(id.vars='Cluster') %>%
  ggplot() +
  aes(x = factor(Cluster), y=value, fill=factor(Cluster)) +
  geom_boxplot() +
#  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~variable, ncol=4, scales='free') +
  theme(legend.position = c(0.87, 0.27))

Ynew %>%
  select(Cluster, colnames(Ynew)[8:14]) %>%
  melt(id.vars='Cluster') %>%
  ggplot() +
  aes(x = factor(Cluster), y=value, fill=factor(Cluster)) +
  geom_boxplot() +
  facet_wrap(~variable, ncol=4, scales='free') +
  theme(legend.position = c(0.87, 0.27))

Ynew %>%
  select(Cluster, colnames(Ynew)[15:21]) %>%
  melt(id.vars='Cluster') %>%
  ggplot() +
  aes(x = factor(Cluster), y=value, fill=factor(Cluster)) +
  geom_boxplot() +
  facet_wrap(~variable, ncol=4, scales='free') +
  theme(legend.position = c(0.87, 0.27))

ComplSet <- allData %>% 
  filter(MP >= 300) %>%
  mutate(X3P. = Ynew[,6], FT. = Ynew[,12], Cluster = kmeanobj$cluster)

for (ii in 1:kclusts){
  curr <- ComplSet[which(kmeanobj$cluster == ii),]
  print("------------------------CLUSTER-----------------------------")
  print(summary(curr))
}

# Player Search
players = c("LeBron", "Kobe", "Stephen Curry", 
            "Durant", "Chris Paul", "Westbrook", 
            "Kawhi", "Harden", "Anthony Davis",
            "Paul George", "Blake Griffin",
            "Carmelo")

for (p in players){
  info <- ComplSet %>%
    filter(grepl(p,Player)) %>%
    select(Player, Pos, Tm, Year, Cluster)
  print(info)
}

setwd("~/Documents/GitHub/AOM-Project")
write.csv(Ynew, file = "TimData.csv")
write.csv(ComplSet, file = "TimAllData.csv")

Isomap <- read.csv("Isomap.csv", header = FALSE)
  
