splitGenres = list()
for (genre in genres) {
  splits <- strsplit(genre, " ")
  for (split in splits) {
    splitGenres <- c(splitGenres, split)  
  }
}

occurences <- table(unlist(splitGenres))
View(occurences)

simpleGenres <- c("pop", "house", "indie", "rock", "hiphop", "rap", "techno", "jazz", "disco", "lo-fi", "indietronica", "r&b", "funk", "edm", "trance", "trap")

for (row)