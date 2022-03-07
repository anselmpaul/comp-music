library(tidyverse)
library(plotly)
library(dplyr)
library(compmus)
library(spotifyr)

getCleanAudioAnalysis <- function(id) {
  result <- get_tidy_audio_analysis(id) %>%
    select(segments) %>%
    unnest(segments) %>%
    select(start, duration, pitches)
  return (result)
    
}

## The Tallis Scholars
tallis <- getCleanAudioAnalysis("2J3Mmybwue0jyQ0UVMYurH")
  
## La Chapelle Royale
chapelle <- getCleanAudioAnalysis("4ccw2IcnFt1Jv9LqQCOYDi")
## The Cambridge Singers
cambridge <- getCleanAudioAnalysis("54cAT1TCFaZbLOB2i1y61h")

## Oxford Camerata
oxford <- getCleanAudioAnalysis("5QyUsMY40MQ1VebZXSaonU")
## Chanticleer
chanticleer <- getCleanAudioAnalysis("1bocG1N8LM7MSgj9T1n3XH")

## The Hilliard Ensemble
hilliard <- getCleanAudioAnalysis("2rXEyq50luqaFNC9DkcU6k")
## The Gabrieli Consort
gabrieli <- getCleanAudioAnalysis("4NnJ4Jes8a8mQUfXhwuITx")

# moby 2ogtMJgCCAOCCNnOXP7FDM
moby <- getCleanAudioAnalysis("2ogtMJgCCAOCCNnOXP7FDM")
# remix 10SS1XiGuycMl3ZH9cdeWF
remix <- getCleanAudioAnalysis("10SS1XiGuycMl3ZH9cdeWF")


# laika 6jRDoJ8srOkUjvn3y7oHko
# laika remix 63SkhUhXqGXo4ojQZ2QqPF
laika <- getCleanAudioAnalysis("6jRDoJ8srOkUjvn3y7oHko")
laikaFelin <- getCleanAudioAnalysis("63SkhUhXqGXo4ojQZ2QqPF")

# spinningPlates
bellaRuse <- getCleanAudioAnalysis("1K6bvVleOIMX5SEuXphEKu")
dashBerlin <- getCleanAudioAnalysis("3pvNvxd3aiDSOwVmqHQoy0")

odesza <- getCleanAudioAnalysis("3xhR3mClWXydDCByJxnOwY")
odeszaLive <- getCleanAudioAnalysis("4X0hePXqBlFkOZ9WWp5W1e")

compmus_long_distance(
  odesza %>% mutate(pitches = map(pitches, compmus_normalise, "manhattan")),
  odeszaLive %>% mutate(pitches = map(pitches, compmus_normalise, "manhattan")),
  feature = pitches,
  method = "euclidean"
) %>%
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
  coord_equal() +
  labs(x = "IPlayYouListen", y = "IPlayYouListen (Live)") +
  theme_minimal() +
  scale_fill_viridis_c(guide = NULL)
