
steam.games$year <- str_sub(steam.games$`date`,-4,-1)
steam.games$year <- as.numeric(as.character(steam.games$year))

library("data.table")

differentgenres <- unique(unlist(splitted.gentes))
matrix(differentgenres)
genrecount <- length(differentgenres)

genre.year.summary <- data.frame(matrix(ncol = 24, nrow = genrecount))
year <- c(sprintf("Year %02d", seq(2000,2021)))
years <- c("genre", year, "Total")
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

fun <- function(genre.year.summary) {
       require(dplyr)
       y <- select_if(genre.year.summary, is_numeric)
       rowSums(y, na.rm=T)
  }

genre.year.summary$Total<-fun(genre.year.summary)
genre.year.shortened <- genre.year.summary[!(genre.year.summary$Total < 200),]
