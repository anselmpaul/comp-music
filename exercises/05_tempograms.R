library(tidyverse)
library(spotifyr)
library(compmus)
library(plotly)

load("data/playlists.RData")
playlist <- topOf2021

test <- get_playlist_audio_features("", "4CUefApxZbdCQS1cWFKIVo")

topOf2021$artist = sapply(playlist$track.artists, function(mat) unlist(mat[3]))
topOf2020$artist = sapply(topOf2020$track.artists, function(mat) unlist(mat[3]))
topOf2019$artist = sapply(topOf2019$track.artists, function(mat) unlist(mat[3]))
topOf2018$artist = sapply(topOf2018$track.artists, function(mat) unlist(mat[3]))
topOf2017$artist = sapply(topOf2017$track.artists, function(mat) unlist(mat[3]))
topOf2016$artist = sapply(topOf2016$track.artists, function(mat) unlist(mat[3]))


topOf2021 %>% ggplot(aes(x = tempo)) + 
  geom_density()

topOf2016 %>% ggplot(aes(x = tempo)) + 
  geom_density()

# slowest
topOf2021 %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2020 %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2019 %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2018 %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2017 %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2016 %>% arrange(tempo) %>% slice(1:10) %>% select(artist, track.name, tempo)

# fastet
topOf2021 %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2020 %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2019 %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2018 %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2017 %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo)
topOf2016 %>% arrange(desc(tempo)) %>% slice(1:10) %>% select(artist, track.name, tempo)

# das zuendet 4yrPLeg5OhHwMhxyMPzSpx
# elf semester 6BZgssH9iQZk8XLt4njw2G

getNovelyPlot <- function(songId) {
  song  <-
    get_tidy_audio_analysis("3uy90vHHATPjtdilshDQDt") %>%
    select(segments) %>%
    unnest(segments)
  
  song  %>%
    mutate(loudness_max_time = start + loudness_max_time) %>%
    arrange(loudness_max_time) %>%
    mutate(delta_loudness = loudness_max - lag(loudness_max)) %>%
    ggplot(aes(x = loudness_max_time, y = pmax(0, delta_loudness))) +
    geom_line() +
    xlim(0, 30) +
    theme_minimal() +
    labs(x = "Time (s)", y = "Novelty")
  
}
 
# getNovelyPlot("4yrPLeg5OhHwMhxyMPzSpx")

# graveoloa 6PJasPKAzNLSOzxeAH33j2

getTempogram <- function(songId) {
  song <- get_tidy_audio_analysis(songId)
    
  song %>%
    tempogram(window_size = 8, hop_size = 1, cyclic = FALSE) %>%
    ggplot(aes(x = time, y = bpm, fill = power)) +
    geom_raster() +
    scale_fill_viridis_c(guide = "none") +
    labs(x = "Time (s)", y = "Tempo (BPM)") +
    theme_classic()
}

getCyclicTempogram <- function(songId) {
  song <- get_tidy_audio_analysis(songId)
  
  song %>%
    tempogram(window_size = 8, hop_size = 1, cyclic = TRUE) %>%
    ggplot(aes(x = time, y = bpm, fill = power)) +
    geom_raster() +
    scale_fill_viridis_c(guide = "none") +
    labs(x = "Time (s)", y = "Tempo (BPM)") +
    theme_classic()
}

# getTempogram("4yrPLeg5OhHwMhxyMPzSpx")
# getCyclicTempogram("4yrPLeg5OhHwMhxyMPzSpx")
# frozen snow 7oC3l3d5t5NUDofzIVipYR
# kaassourfflee 1tJmVBavhzhCvTAYHKZ6Rn

# this might be interesting 
# quick musical doodles  7pe2OKY04MW1Cunyqrai83
#getNovelyPlot("7pe2OKY04MW1Cunyqrai83")

tempoGram <- getTempogram("7pe2OKY04MW1Cunyqrai83")
cyclicTempoGram <- getCyclicTempogram("7pe2OKY04MW1Cunyqrai83")

allTops <- rbind(topOf2016, topOf2017, topOf2018, topOf2019, topOf2020, topOf2021)
bpmHisto <- ggplotly(ggplot(allTops, aes(fill=playlist_name , x=tempo)) + 
              geom_histogram() +
             facet_grid(rows = vars(playlist_name)))
bpmHisto
