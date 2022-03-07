library(tidyverse)
library(spotifyr)
library(compmus)


# kleine joengen     6vzj8d0yOzg3ELH2T4u0CC
# zij geloft in mij  4GrQPpjaZmBf67b9Uh83T8
# bzt                5ZLkc5RY1NM4FtGWEd6HOE
# #4                 2x7VFpoHN8eutpliLKFfuh
#le freak
# are u mine https://open.spotify.com/track/29tzJGvqJPTAFs6LXmsHoA?si=d1360461537d4f51
# whyd you only call me in the night https://open.spotify.com/track/48q0vSHcJdhK3IiXH8C5WJ?si=37c46b2692e24460
# booty swing https://open.spotify.com/track/3sMleqdCDalZ6xsAQe8xuY?si=3214f24230434604


flashAnalysis <-
  get_tidy_audio_analysis("52d8baQxvV3AkHcAyXCkkO") %>% # Change URI.
  compmus_align(sections, segments) %>%                     # Change `bars`
  select(sections) %>%                                      #   in all three
  unnest(sections) %>%                                      #   of these lines.
  mutate(
    pitches =
      map(segments,
          compmus_summarise, pitches,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  ) %>%
  mutate(
    timbre =
      map(segments,
          compmus_summarise, timbre,
          method = "rms", norm = "euclidean"              # Change summary & norm.
      )
  )

# bzt %>%
#   compmus_gather_timbre() %>%
#   ggplot(
#     aes(
#       x = start + duration / 2,
#       width = duration,
#       y = basis,
#       fill = value
#     )
#   ) +
#   geom_tile() +
#   labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
#   scale_fill_viridis_c() +                              
#   theme_classic()


pitchesSSMFlashPlotSections <- flashAnalysis %>%
  compmus_self_similarity(pitches, "angular") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  ggtitle("Pitches") +
  labs(x = "", y = "")

timbreSSMFlashPlotSections <- flashAnalysis %>%
  compmus_self_similarity(timbre, "angular") %>% 
  ggplot(
    aes(
      x = xstart + xduration / 2,
      width = xduration,
      y = ystart + yduration / 2,
      height = yduration,
      fill = d
    )
  ) +
  geom_tile() +
  coord_fixed() +
  scale_fill_viridis_c(guide = "none") +
  theme_classic() +
  ggtitle("Timbre") +
  labs(x = "", y = "")

test <- pitchesSSMFlashPlotBeats + 
  annotate(
  "text",
  x = 55, y = 0,
  label = "←  synth stops",
  vjust = 1, size = 3, color = "#FF0000"
)
test
#timbreSSMFlashPlotBeats
#timbreSSMFlashPlotSections
