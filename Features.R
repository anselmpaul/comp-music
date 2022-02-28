load("playlists.RData")
library(ggplot2)
library(plotly)

topOf2016$year <- 2016
topOf2017$year <- 2017
topOf2018$year <- 2018
topOf2019$year <- 2019
topOf2020$year <- 2020
topOf2021$year <- 2021

topTen2016 <- head(topOf2016, 10)
topTen2017 <- head(topOf2017, 10)
topTen2018 <- head(topOf2018, 10)
topTen2019 <- head(topOf2019, 10)
topTen2020 <- head(topOf2020, 10)
topTen2021 <- head(topOf2021, 10)

allTopTen <- rbind(topTen2016, topTen2017, topTen2018, topTen2019, topTen2020, topTen2021)
# danceability
# energy
# key 
# loudness
# mode
# speechiness
# acousticness
# instrumentalness
# liveness
# tempo
# valence

#timeCapsule <- get_playlist_audio_features("", "5hb3DGs2AGTl4jfXzM6lRF")
timeCapsule$rank <- 1:nrow(timeCapsule)

allTops <- rbind(topOf2016, topOf2017, topOf2018, topOf2019, topOf2020, topOf2020, topOf2021)

# , size= (100 - rank)
featuresPlot <- ggplot(allTops, aes(rank, valence, color=playlist_name)) +
  geom_point(alpha= 0.6,  size=3) #+
  #geom_line()

ggplotly(featuresPlot)

  