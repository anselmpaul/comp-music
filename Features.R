
topOf2016$rank <- 1:nrow(topOf2016)
topOf2017$rank <- 1:nrow(topOf2017)
topOf2018$rank <- 1:nrow(topOf2018)
topOf2019$rank <- 1:nrow(topOf2019)
topOf2020$rank <- 1:nrow(topOf2020)
topOf2021$rank <- 1:nrow(topOf2021)

allTops <- rbind(topOf2016, topOf2017, topOf2018, topOf2019, topOf2020, topOf2021)

ggplot(allTops, aes(valence, instrumentalness, color=playlist_name)) +
  geom_point(alpha = (rank / 100 ))
