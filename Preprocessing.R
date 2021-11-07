# setwd("~/R/RFiles/DMProject/")

#Import files
steam.charts <- read.csv(file=file.path(".", "Data", "SteamCharts.csv"))
steam.sales <- read.csv(file=file.path(".", "Data", "Steam_games_final.csv"))
steam.games <- read.csv(file.path(".", "Data", "final_data_new.csv"))

#Rename name columns
names(steam.sales)[names(steam.sales) == "Name"] <- "name"
names(steam.charts)[names(steam.charts) == "gamename"] <- "name"

#Clean name column
steam.games$name <- tolower(gsub("[^[:alnum:] ]", "", steam.games$name))
steam.sales$name <- tolower(gsub("[^[:alnum:] ]", "", steam.sales$name))
steam.charts$name <- tolower(gsub("[^[:alnum:] ]", "", steam.charts$name))

# https://stackoverflow.com/questions/1299871/how-to-join-merge-data-frames-inner-outer-left-right
#Join columns
steam.merged.games.charts <- merge(x = steam.games, y = steam.charts, by = "name")
steam.merged.games.sales <- merge(x = steam.games, y = steam.sales, by = "name")
steam.merged.charts.sales <- merge(x = steam.charts, y = steam.sales, by = "name")
steam.merged.all <- merge(x = steam.games, y=steam.merged.charts.sales, by = "name")
