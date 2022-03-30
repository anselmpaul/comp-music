library(tidyverse)
library(spotifyr)
library(compmus)
load("data/playlists.RData")

getPlaylistAudioAnalysis <- function(playlist) {
  playlist %>%
    #slice(1:30) %>%
    add_audio_analysis()
}

audio2016 <- getPlaylistAudioAnalysis(topOf2016) %>% mutate(playlist = "2016")
audio2021 <- getPlaylistAudioAnalysis(topOf2021) %>% mutate(playlist = "2021")
audioTC <- getPlaylistAudioAnalysis(timeCapsule) %>% mutate(playlist = "TimeCapsule")

audioTC$year <- 0

# mix <- rbind(
#   audioTC, 
#   audio2021, 
#   audio2016)

tc2021 <- rbind(audioTC, audio2021)
tc2016 <- rbind(audioTC, audio2016)
years <- rbind(audio2016, audio2021)
tc2021$plot <- "TimeCapsule + 2021"
tc2016$plot <- "TimeCapsule + 2016"
years$plot <- "2016 + 2021"

test <- rbind(tc2021, tc2016, years)

getSdPlot <- function(playlist) {
  playlist %>%
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
      colour = playlist,
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
    colour = "Playlist",
    size = "Duration (min)",
    alpha = "Volume (dBFS)"
  ) +
  facet_grid(rows = vars(plot))
}


getSdPlot(test)

