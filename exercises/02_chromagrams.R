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
    compmus_gather_chroma() -> chroma
  
  return(chroma)
    
}

# wood "6IQILcYkN2S2eSu5IHoPEH"
# darkTheme 6qMcBfQ3Dy3Cn6q27tcQDR

# fernsehturm 1VCuqOYsvdibOB1UEPp4P0
# highHopes 6ow0fcGduUwyvUO2iVlb1j
# moby 2ogtMJgCCAOCCNnOXP7FDM
# remix 10SS1XiGuycMl3ZH9cdeWF
#p1 <- getChroma("2ogtMJgCCAOCCNnOXP7FDM", "euclidean")
#p2 <- getChroma("10SS1XiGuycMl3ZH9cdeWF", "euclidean")
#grid.arrange(p1, p2, ncol = 2)

# iplayyoulisten
#getChroma("3xhR3mClWXydDCByJxnOwY", "euclidean")
# iplayyoulistenLIVE
#getChroma("4X0hePXqBlFkOZ9WWp5W1e", "euclidean")

# you help me loose my mind 2LTIrVBonJJLQHAjw41b28
# neverLearn 07AeMia4F72sDld3PUvLWj
# my life  2NDZ6i6UfOUSKgFiTQKbnv
# high hopes 6ow0fcGduUwyvUO2iVlb1j
# flash 52d8baQxvV3AkHcAyXCkkO
# fernsehturm 1VCuqOYsvdibOB1UEPp4P0


bestOfChromas <- tibble(
  years = c(2016, 2017, 2018, 2019, 2020, 2021),
  chromas = list(
    getChroma("2LTIrVBonJJLQHAjw41b28", "euclidean"),
    getChroma("07AeMia4F72sDld3PUvLWj", "euclidean"),
    getChroma("2NDZ6i6UfOUSKgFiTQKbnv", "euclidean"),
    getChroma("6ow0fcGduUwyvUO2iVlb1j", "euclidean"),
    getChroma("52d8baQxvV3AkHcAyXCkkO", "euclidean"),
    getChroma("1VCuqOYsvdibOB1UEPp4P0", "euclidean")
))

test <- unnest(bestOfChromas[1, "chromas"])
max(unnest(bestOfChromas[1, "chromas"])$start) + tail(unnest(bestOfChromas[1, "chromas"])$duration, n=1)

ggplot(unnest(bestOfChromas[3, "chromas"]),
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
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = round(seq(
    0, 
    max(unnest(bestOfChromas[3, "chromas"])$start) + tail(unnest(bestOfChromas[3, "chromas"])$duration, n=1), 
    by = 10),1))

                     