library(readxl)
library(tidyverse)
library(stringr)

setwd("~/Dropbox (MIT)/AOM Project/Data/All Data")
data.files = list.files(pattern="*.xlsx")

data = data.frame()
for (i in data.files) {
  year = substr(i,start=1,stop = 4) %>% as.numeric()
  temp = read_excel(i) %>%
    mutate(Year = year,
           Player = str_extract(Player, pattern = "^[A-Za-z\\s-\\.]+"))
  data = rbind(data, temp)
}
write.csv(data, file = "Combined data.csv")



