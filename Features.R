load("playlists.RData")
library(dplyr)
library(ggplot2)
library(plotly)
library(GGally)
library(grid)
library(tidyverse)
library(gridExtra)


topOf2016$year <- 2016
topOf2017$year <- 2017
topOf2018$year <- 2018
topOf2019$year <- 2019
topOf2020$year <- 2020
topOf2021$year <- 2021
timeCapsule$year <- "timeCapsule"

allTops <- rbind(topOf2016, topOf2017, topOf2018, topOf2019, topOf2020, topOf2021)
test <- allTops %>% group_by("rank")

## danceability
## energy
# key 
# loudness
# mode
## speechiness
# acousticness
# instrumentalness
## liveness
# tempo
## valence

ggplot(allTops, aes())

getMediansOfPlaylist <- function(playlist) {
  medianPlaylist <- playlist %>%
    summarize(
      medianDance = median(danceability),
      medianEnergy = median(energy),
      medianSpeech = median(speechiness),
      medianLive = median(liveness),
      medianValence = median(valence),
      year = max(year)
    )
}

median2016 <- getMediansOfPlaylist(topOf2016)
median2017 <- getMediansOfPlaylist(topOf2017)
median2018 <- getMediansOfPlaylist(topOf2018)
median2019 <- getMediansOfPlaylist(topOf2019)
median2020 <- getMediansOfPlaylist(topOf2020)
median2021 <- getMediansOfPlaylist(topOf2021)
medianTC <- getMediansOfPlaylist(timeCapsule)

allMedians <- rbind(median2016, median2017, median2018, median2019, median2020, median2021, medianTC)

p <- ggparcoord(allMedians,
                columns = 1:5, groupColumn = 6,
                scale="uniminmax",
)

#ggplotly(p)

p1 <- ggplot(allMedians, aes(year, medianDance)) +
  ylim(0, 1) +
  geom_line(aes(x=year, y=medianDance, group = 1, color="Danceability")) +
  geom_line(aes(x=year, y=medianEnergy, group = 1, color="Energy")) +
  geom_line(aes(x=year, y=medianSpeech, group = 1, color="Speechiness")) +
  geom_line(aes(x=year, y=medianLive, group = 1, color="Liveness")) +
  geom_line(aes(x=year, y=medianValence, group = 1, color="Valence"))

getAveragePlaylist <- function(playlist) {
  avgPlaylist <- playlist %>%
    summarize(
      avgDance = mean(danceability),
      avgEnergy = mean(energy),
      avgLiveness = mean(liveness),
      avgSpeech = mean(speechiness),
      avgValence = mean(valence),
      year = max(year)
    )
  
  return(avgPlaylist)
}

avg2016 <- getAveragePlaylist(topOf2016)
avg2017 <- getAveragePlaylist(topOf2017)
avg2018 <- getAveragePlaylist(topOf2018)
avg2019 <- getAveragePlaylist(topOf2019)
avg2020 <- getAveragePlaylist(topOf2020)
avg2021 <- getAveragePlaylist(topOf2021)
avgTC <- getAveragePlaylist(timeCapsule)

allAvg = rbind(avg2016, avg2017, avg2018, avg2019, avg2020, avg2021, avgTC)

p2 <- ggplot(allAvg, aes(year)) +
  ylim(0, 1) + 
  geom_line(aes(x=year, y=avgDance, group = 1, color="Danceability")) +
  geom_line(aes(x=year, y=avgEnergy, group = 1, color="Energy")) +
  geom_line(aes(x=year, y=avgSpeech, group = 1, color="Speechiness")) +
  geom_line(aes(x=year, y=avgLiveness, group = 1, color="Liveness")) +
  geom_line(aes(x=year, y=avgValence, group = 1, color="Valence")) 

p2
#grid.arrange(p1, p2, ncol = 2)