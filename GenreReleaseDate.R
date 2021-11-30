
steam.games$year <- str_sub(steam.games$`date`,-4,-1)
steam.games$year <- as.numeric(as.character(steam.games$year))

library("collections")
library("data.table")

differentgenres <- unique(unlist(splitted.gentes))
matrix(differentgenres)
a <- length(differentgenres)

genre.year.summary <- data.frame(matrix(ncol = 23, nrow = a))
year <- c(sprintf("Year %02d", seq(2000,2021)))
years <- c("genre", year)
colnames(genre.year.summary) <- years
genre.year.summary$genre <- differentgenres

steam.games.genre.year <- select(steam.games, c("popu_tags","year"))
for (i in 2000:2021) {
  counter = 1
  result <- filter(steam.games.genre.year, year == i)
  for (j in genre.year.summary$genre) {
    genre.year.summary[counter,i-1998] = sum(grepl( j, result$popu_tags, fixed = TRUE))
    counter = counter + 1
  }
  print(i)
}
genre.year.summary.2 = transpose(genre.year.summary)
