library(tidyverse)
library(spotifyr)
library(plotly)

# get the playlists
#topOfAllTime <- get_playlist_audio_features("", "56aQOR4NRbXoxaYuKfiGe4")
topOf2016 <- get_playlist_audio_features("", "0aiaCO6mk6bEPPDErC9oFp")
topOf2017 <- get_playlist_audio_features("", "1xgUVZdrris3opd6rW6TSX")
topOf2018 <- get_playlist_audio_features("", "1mgY2xI49CWUfBpNzzidQO")
topOf2019 <- get_playlist_audio_features("", "6pHwUhsAgex7HmE53zulm4")
topOf2020 <- get_playlist_audio_features("", "4MMsv9uHzs4yr8YAIRMPfD")
topOf2021 <- get_playlist_audio_features("", "79X273LI34tXLJRXNLIiIz")
timeCapsule <- get_playlist_audio_features("", "5hb3DGs2AGTl4jfXzM6lRF")


getSingleArtistGenres = function(artistId) {
  artist <- get_artist(artistId)
  return(artist$genres)
}

getSongGenres = function(artists) {
  artistsIds <- getArtistsIds(artists)
  songGenres = list()
  for (id in artistsIds) {
    songGenres <- append(songGenres, getSingleArtistGenres(id))
  }
  return(songGenres)
}

getArtistsIds = function(artists) {
  listOfArtists = list()
  for (artist in artists) {
    artistId = artist$id
    listOfArtists <- append(listOfArtists, artist$id)
  }  
  return(listOfArtists)
}

getGenresOfYear = function(playlist, year) {
  genresOfYear = list()
  for (row in 1:nrow(playlist)) {
    artists <- playlist[row,]$track.artists
    genresOfYear <- append(genresOfYear, getSongGenres(artists))
  }
  countedGenresOfYear <- as.data.frame(table(unlist(genresOfYear))) %>%
    mutate(Year = year) %>%
    mutate(genre = Var1) %>%
    mutate(count = Freq)
}

genres2021 = getGenresOfYear(topOf2021, 2021)
genres2020 = getGenresOfYear(topOf2020, 2020)
genres2019 = getGenresOfYear(topOf2019, 2019)
genres2018 = getGenresOfYear(topOf2018, 2018)
genres2017 = getGenresOfYear(topOf2017, 2017)
genres2016 = getGenresOfYear(topOf2016, 2016)
genresTimeCapsule = getGenresOfYear(timeCapsule, "timeCapsule")

allGenres = rbind(genres2016, genres2017, genres2018, genres2019, genres2020, genres2021, genresTimeCapsule)

## PLOT
library(plotly)

plot <- ggplot(allGenres, aes(Year, count, fill=genre)) +
  geom_bar(position="fill", stat="identity") +
  # geom_line(numberOfGenresPerYear, aes(Year, genreCount), stat="identity") +
  theme(
    legend.position = "none",
    #axis.text.y = element_blank(),
  ) +
  ylab("Genres") +
  scale_y_continuous(sec.axis = sec_axis(~.*180))

interactivePlot <- ggplotly(plot)
interactivePlot

## collect data about genres
# genres <- allGenres$genre
# 
# numberOfGenresPerYear = data.frame(Year = c(2016, 2017, 2018, 2019, 2020, 2021),
#                                    genreCount = c(nrow(genres2016), nrow(genres2017), nrow(genres2018), nrow(genres2019), nrow(genres2020), nrow(genres2021)))
# 
# p <- ggplot(numberOfGenresPerYear, aes(Year, genreCount)) +
#   geom_line()
# 
# ggplotly(p)
