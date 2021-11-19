# setwd("~/R/RFiles/DMProject/")
# library(usethis)
# use_github(protocol='https', auth_token = Sys.getenv("GITHUB_PAT"))

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
# steam.merged.games.charts <- merge(x = steam.games, y = steam.charts, by = "name")
# steam.merged.games.sales <- merge(x = steam.games, y = steam.sales, by = "name")
# steam.merged.charts.sales <- merge(x = steam.charts, y = steam.sales, by = "name")
# steam.merged.all <- merge(x = steam.games, y=steam.merged.charts.sales, by = "name")

#https://stackoverflow.com/questions/35113553/r-remove-first-character-from-string/35113673
#özel karakterleri çýkardým, boþluklarý çýkardým çünkü ilk genre sürekli boþluksuz gözüküyordu direk datanýn içinde, ve baþa boþluk eklemek daha zor
steam.games$popu_tags = sub('^.|.$',"", steam.games$popu_tags)
steam.games$popu_tags <- gsub("'", "", steam.games$popu_tags)
steam.games$popu_tags <- gsub(" ", "", steam.games$popu_tags)
steam.games$popu_tags <- gsub("]$", "", steam.games$popu_tags)
steam.games$popu_tags <- gsub("\\+$", "", steam.games$popu_tags)

splitted.gentes <- strsplit(steam.games$popu_tags, ",")

splitted.gentes <- lapply(splitted.gentes, function (x){
  x <- unique(x)
  # Turn-based
  x <- x[!x == "Based"]
  # FPS - TPS etc...
  x <- x[!x == "Person"]
  # Rich Presence
  x <- x[!x == "Rich"]
  # Great Soundtrack, Great Combat etc...
  x <- x[!x == "Great"]
  # Free to Play
  x <- x[!x == "Play"]
  # Early Access
  x <- x[!x == "Access"]
  # Open World
  x <- x[!x == "World"]
  # Real-Time
  x <- x[!x == "Time"]
  
  x[x == "Early"] = "Early Access"
  x[x == "Turn-"] = "Turn-Based"
  x[x == "First-"] = "First-Person Shooter"
  x[x == "Third-" || x == "Third"] = "Third-Person Shooter"
  x[x == "Hero"] = "Hero Shooter"
  x[x == "Team-"] = "Team-Based"
  x[x == "Party-"] = "Party-Based"
  x[x == "Fast-"] = "Fast-Paced"
  x[x == "Free"] = "Free To Play"
  x[x == "Open"] = "Open World"
  x[x == "Massively"] = "MMO"
  x[x == "Class-"] = "Class-Based"
  x[x == "Base"] = "Base Building"
  x[x == "Looter"] = "Looter Shooter"
  x[x == "Real-"] = "Real-time"

  x
})

steam.all.genres <- as.factor(unlist(splitted.gentes))
steam.games$popu_tags <- as.list(splitted.gentes)


steam.games$categories = sub('^.|.$',"", steam.games$categories)
steam.games$categories <- gsub("'", "", steam.games$categories)
steam.games$categories <- gsub(" ", "", steam.games$categories)
steam.games$categories <- gsub("]$", "", steam.games$categories)

splitted.categories <- strsplit(steam.games$categories, ",")

steam.all.categories <- as.factor(unlist(splitted.categories))
steam.games$categories <- as.list(splitted.categories)

#https://stackoverflow.com/questions/24256044/comma-separated-string-to-list-in-r
#Senin attýðýn, listeye çeviriyor

#https://stackoverflow.com/questions/4227223/convert-a-list-to-a-data-frame
#list'i data'ya çeviriyor ama iþimize yarar mý zorlaþtýrýr mý bilemedim - kolon isimlerine edit gerek
# steam.genre.data <- do.call(rbind.data.frame, steam.genre.list)

