load("data/playlists.RData")
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

# do this for each playlist to get index/ (=rank) as value
# topOf2021$rank <- 1:nrow(topOf2021)

allTops <- rbind(topOf2016, topOf2017, topOf2018, topOf2019, topOf2020, topOf2021)


getPlot <- function(feature) {
  plot <- ggplot(allTops, aes_string(x ="rank", y=feature)) +
    geom_point() +
    facet_grid(rows = vars(year)) +
    geom_smooth(method = "lm")
  return (plot)
}


danceabilityPlot <- getPlot("danceability")
energyPlot <- getPlot("energy")
keyPlot <- getPlot("key")
loudnessPlot <- getPlot("loudness")
modePlot <- getPlot("mode")
speechinessPlot <- getPlot("speechiness")
acousticnessPlot <- getPlot("acousticness")
instrumentalnessPlot <- getPlot("instrumentalness")
livenessPlot <- getPlot("liveness")
tempoPlot <- getPlot("tempo")
valencePlot <- getPlot("valence")

danceabilityPlot
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