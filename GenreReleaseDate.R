
steam.games$year <- str_sub(steam.games$`date`,-4,-1)
steam.games$year <- as.numeric(as.character(steam.games$year))

library("data.table")

different.genres <- c("Building","Tactical","Puzzle","Adventure","Platform", "Shooter", "Fighting", "Stealth", "Survival", "Rhythm", "Battle", "Interactive", "Roguelike","Simulation", "Clicker", "Horror", "Auto", "Strategy", "Tower Defense", "Grand", "Sports", "Open World", "Sandbox", "Total (Other Genres Included)")

genre.year.percentages <- data.frame(matrix(ncol = 23, nrow = length(different.genres)))
genre.year.summary <- data.frame(matrix(ncol = 23, nrow = length(different.genres)))
year <- c(sprintf("Year %02d", seq(2000,2020)))
years <- c("genre", year, "Total")
colnames(genre.year.percentages) <- years
genre.year.percentages$genre <- different.genres
colnames(genre.year.summary) <- years
genre.year.summary$genre <- different.genres


steam.games.genre.year <- select(steam.games, c("popu_tags","year"))
for (i in 2000:2020) {
  counter = 1
  result <- filter(steam.games.genre.year, year == i)
  for (j in genre.year.summary$genre) {
    genre.year.summary[counter,i-1998] = sum(grepl( j, result$popu_tags, fixed = TRUE))
    counter = counter + 1
  }
  genre.year.summary[length(different.genres),i-1998] = nrow(result)
}

fun <- function(genre.year.summary) {
       require(dplyr)
       y <- select_if(genre.year.summary, is_numeric)
       rowSums(y, na.rm=T)
  }

genre.year.summary$genre <- gsub("Battle", "Battle Royale", genre.year.summary$genre)
genre.year.summary$genre <- gsub("Auto", "AutoBattler", genre.year.summary$genre)
genre.year.summary$genre <- gsub("Grand", "Grand Strategy", genre.year.summary$genre)

genre.year.summary$Total<-fun(genre.year.summary)

result <- filter(steam.games, developer == "Paradox Development Studio")

options(digits = 2)
for (i in 1:22) {
  for (j in 1:length(different.genres)) {
    genre.year.percentages[j,i+1] = (genre.year.summary[j,i+1]/genre.year.summary[24,i+1])*100
  }
}
