library(tidyverse)
library(spotifyr)
library(compmus)
library(grid)
library(gridExtra)

getChroma = function(songId, mode) {
  song <-
    get_tidy_audio_analysis(songId) %>%
    select(segments) %>%
    unnest(segments) %>%
    select(start, duration, pitches)
  
  song %>%
    mutate(pitches = map(pitches, compmus_normalise, mode)) %>%
    compmus_gather_chroma() %>% 
    ggplot(
      aes(
        x = start + duration / 2,
        width = duration,
        y = pitch_class,
        fill = value
      )
    ) +
    geom_tile() +
    labs(x = "Time (s)", y = NULL, fill = "Magnitude") +
    theme_minimal() +
    scale_fill_viridis_c()
}

# wood "6IQILcYkN2S2eSu5IHoPEH"
# darkTheme 6qMcBfQ3Dy3Cn6q27tcQDR

# fernsehturm 1VCuqOYsvdibOB1UEPp4P0
# highHopes 6ow0fcGduUwyvUO2iVlb1j
# moby 2ogtMJgCCAOCCNnOXP7FDM
# remix 10SS1XiGuycMl3ZH9cdeWF
p1 <- getChroma("2ogtMJgCCAOCCNnOXP7FDM", "euclidean")
p2 <- getChroma("10SS1XiGuycMl3ZH9cdeWF", "euclidean")
#grid.arrange(p1, p2, ncol = 2)

# iplayyoulisten
getChroma("3xhR3mClWXydDCByJxnOwY", "euclidean")
# iplayyoulistenLIVE
getChroma("4X0hePXqBlFkOZ9WWp5W1e", "euclidean")
