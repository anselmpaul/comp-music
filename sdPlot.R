library(tidyverse)
library(spotifyr)
library(compmus)


getPlaylistAudioAnalysis <- function(id) {
  get_playlist_audio_features(
    "anselmpaul",
    id
  ) %>%
    #slice(1:30) %>%
    add_audio_analysis()
}

audio2016 <- getPlaylistAudioAnalysis("0aiaCO6mk6bEPPDErC9oFp") %>% mutate(genre = "2016")
audio2021 <- getPlaylistAudioAnalysis("79X273LI34tXLJRXNLIiIz") %>% mutate(genre = "2021")
audioTC <- getPlaylistAudioAnalysis("5hb3DGs2AGTl4jfXzM6lRF") %>% mutate(genre = "TimeCapsule")

mix <- rbind(
  audioTC, 
  audio2021, 
  audio2016)

getSdPlot <- function() {
  mix %>%
  mutate(
    sections =
      map(
        sections,                                    # sections or segments
        summarise_at,
        vars(tempo, loudness, duration),             # features of interest
        list(section_mean = mean, section_sd = sd)   # aggregation functions
      )
  ) %>%
  unnest(sections) %>%
  ggplot(
    aes(
      x = tempo,
      y = tempo_section_sd,
      colour = genre,
      alpha = loudness
    )
  ) +
  geom_point(aes(size = duration / 60)) +
  geom_rug() +
  theme_minimal() +
  ylim(0, 5) +
  labs(
    x = "Mean Tempo (bpm)",
    y = "SD Tempo",
    colour = "Genre",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  ) +
  facet_grid(rows = vars(genre))
}

getSdPlot()
