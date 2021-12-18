library(tidyverse)
# https://stackoverflow.com/questions/36924911/how-to-assign-a-value-to-a-data-frame-filtered-by-dplyr
# https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
categorize.data.func <- function(data, histogram, column, new_label){
  data[new_label] <- NA
  cnt <- 0
  mapply(function(x, y) {
    cnt <<- cnt + 1
    data <<- mutate(data, "{new_label}":=ifelse(
      !!as.symbol(column) >= x & !!as.symbol(column) <= y, 
      cnt, !!as.symbol(new_label)))
  } ,x=histogram[["data"]][[1]][["xmin"]],
  y=histogram[["data"]][[1]][["xmax"]])
  data
}

compile.genre.func <- function(data, genre.list){
  genre.list <- gsub(" ", "_", genre.list)
  print(genre.list)
  (data[c("name", "popu_tags")] 
   %>% unnest_longer(popu_tags) 
   %>% filter(popu_tags %in% genre.list) 
   %>% table 
   %>% as.data.frame.matrix 
   %>% mutate(across(, as.logical)) 
   %>% rownames_to_column("name"))
}

#stickiness
steam.charts.scores <- (steam.charts 
                            %>% group_by(name, year) 
                            %>% summarise(peak_yau=max(peak))
                            %>% ungroup()
                            %>% merge(steam.charts, by=c('name','year'))
                            %>% transform(stickiness=(avg / peak_yau)))

steam.charts.scores <- (steam.charts.scores 
                        %>% group_by(name)
                        %>% summarise(stickiness_avg=mean(stickiness, na.rm = TRUE))
                        %>% ungroup()
                        %>% merge(steam.charts.scores, by='name'))

steam.charts.scores <- (steam.charts.scores 
                        %>% group_by(name)
                        %>% summarise(stickiness_sd=sd(stickiness, na.rm = TRUE))
                        %>% ungroup()
                        %>% merge(steam.charts.scores, by='name'))

#Trendiness / Obsoleteness
steam.charts.scores <- (steam.charts 
                            %>% group_by(name)
                            %>% summarise(min_gain=min(gain, na.rm = TRUE),
                                          max_gain=max(gain, na.rm = TRUE), 
                                          max_peak=max(peak))
                            %>% ungroup()
                            %>% merge(steam.charts.scores, by='name')
                            %>% transform(trendiness=(max_gain / max_peak),
                                          obsoleteness=(min_gain / max_peak)))
# Outlier management
steam.charts.scores$stickiness_avg[steam.charts.scores$stickiness_avg < 0.1] <- 0.1
steam.charts.scores$stickiness_avg[steam.charts.scores$stickiness_avg >= 0.4] <- 0.4

steam.charts.scores$stickiness_sd[steam.charts.scores$stickiness_sd < 0.05] <- 0.05
steam.charts.scores$stickiness_sd[steam.charts.scores$stickiness_sd >= 0.17] <- 0.17

steam.charts.scores$trendiness[steam.charts.scores$trendiness < 0] <- 0
steam.charts.scores$trendiness[steam.charts.scores$trendiness >= 0.3] <- 0.3

steam.charts.scores$obsoleteness[steam.charts.scores$obsoleteness < -0.25] <- -0.25
steam.charts.scores$obsoleteness[steam.charts.scores$obsoleteness >= -0.05] <- -0.05

steam.charts.scores.condensed <- steam.charts.scores %>% group_by(name) %>% summarise(
  name=first(name), 
  stickiness_avg=first(stickiness_avg),
  stickiness_sd=first(stickiness_sd),
  trendiness=first(trendiness), 
  obsoleteness=first(obsoleteness))


his.stickiness.avg <- (ggplot(steam.charts.scores.condensed, aes(x=stickiness_avg)) 
  + geom_histogram(bins = 7,fill = "white", color="Purple" )
  + stat_bin(aes(y=..count.., label=..count..), geom="text",bins = 7))  

his.stickiness.sd <- (ggplot(steam.charts.scores.condensed, aes(x=stickiness_sd)) 
  + geom_histogram(bins = 7,fill = "white", color="Pink" )
  + stat_bin(aes(y=..count.., label=..count..), geom="text",bins = 7))  

his.trendiness <- (ggplot(steam.charts.scores.condensed, aes(x=trendiness)) 
  + geom_histogram(bins = 7,fill = "white", color="Green" )
  + stat_bin(aes(y=..count.., label=..count..), geom="text",bins = 7)) 

his.obsoleteness <- (ggplot(steam.charts.scores.condensed, aes(x=obsoleteness)) 
  + geom_histogram(bins = 7,fill = "white", color="Red" )
  + stat_bin(aes(y=..count.., label=..count..), geom="text",bins = 7))  

his.stickiness.avg <- ggplot_build(his.stickiness.avg)
his.stickiness.sd <- ggplot_build(his.stickiness.sd)
his.trendiness <- ggplot_build(his.trendiness)
his.obsoleteness <- ggplot_build(his.obsoleteness)

#print(his.stickiness.avg)
#print(his.stickiness.sd)
#print(his.trendiness)
print(his.obsoleteness)

training.data <- (steam.charts.scores.condensed[c("name", 
                                         "stickiness_avg", 
                                         "stickiness_sd", 
                                         "trendiness", 
                                         "obsoleteness")]
                    %>% merge(compile.genre.func(steam.games, c("Indie","Building","Tactical","Puzzle",
                                                                "Adventure", "Action", "Singleplayer", "Multiplayer", "Shooter",  "Survival", 
                                                                "Roguelike","Simulation",
                                                                "Horror", "Strategy",  "Strategy", "Atmospheric", "Fantasy", "Violent",
                                                                "Sports", "Open World", "Sandbox")), by="name"))

training.data.factor <- categorize.data.func(steam.charts.scores.condensed, his.stickiness.avg, "stickiness_avg" , "stickiness_avg_factor")
training.data.factor <- categorize.data.func(training.data.factor, his.stickiness.sd, "stickiness_sd" , "stickiness_sd_factor")
training.data.factor <- categorize.data.func(training.data.factor, his.trendiness, "trendiness" , "trendiness_factor")
training.data.factor <- categorize.data.func(training.data.factor, his.obsoleteness, "obsoleteness" , "obsoleteness_factor")

training.data.factor$stickiness_avg_factor <- as.factor(training.data.factor$stickiness_avg_factor)
training.data.factor$stickiness_sd_factor <- as.factor(training.data.factor$stickiness_sd_factor)
training.data.factor$trendiness_factor <- as.factor(training.data.factor$trendiness_factor)
training.data.factor$obsoleteness_factor <- as.factor(training.data.factor$obsoleteness_factor)

training.data.factor <- (training.data.factor[c("name", 
                                                  "stickiness_avg_factor", 
                                                  "stickiness_sd_factor", 
                                                  "trendiness_factor", 
                                                  "obsoleteness_factor")]
                  %>% merge(compile.genre.func(steam.games, c("Indie","Building","Tactical","Puzzle",
                                                              "Adventure", "Action", "Singleplayer", "Multiplayer", "Shooter",  "Survival", 
                                                              "Roguelike","Simulation",
                                                              "Horror", "Strategy", "Atmospheric", "Fantasy", "Violent",
                                                               "Sports", "Open World", "Sandbox")), by="name"))

print(str(training.data.factor))
print(table(training.data.factor$stickiness_sd_factor))

# print(filter(test.data, rowSums(test.data[,-1]) == 1))
# table(training.data.factor[,-1:-5]$Puzzle)
# print(rowSums(training.data.factor[,-1:-5]))
