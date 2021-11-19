library(tidyverse)

# https://stackoverflow.com/questions/48350991/filter-a-dataframe-by-values-in-a-column-of-type-list

#get.game.counts.by.genre <- function(genre)
#  print(genre)
#  filter(steam.games, map_lgl(steam.games$popu_tags, ~"genre" %in% .))
steam.merged.games.charts <- merge(x = steam.games, y = steam.charts, by = "name")
steam.merged.games.charts$gain<- as.numeric(steam.merged.games.charts$gain)
steam.merged.games.charts$gain[is.na(steam.merged.games.charts$gain)] <- 0.0

top.genres <- (steam.merged.games.charts 
               %>% group_by(name) 
               %>% summarise(
                 genre=first(popu_tags), 
                             avg = mean(avg),
                             peak = max(peak), 
                             gain = mean(gain)) 
               %>% group_by(genre) 
               %>% summarise(
                             genre=first(genre), 
                             count = n(),
                             avg = sum(avg),
                             peak = mean(peak), 
                             gain = mean(gain)))

top.genres[,3:5] <- top.genres[,3:5] / 1000.0
# Top average player count
top.genres <- arrange(top.genres, desc(avg))
(ggplot(data=top.genres[1:20,], aes(x=genre , y=avg)) 
  + geom_bar(stat="identity") 
  + geom_text(aes(label = count),  vjust = 1.5, colour = "white")
  + labs(x="Genre", y="Average Player Count (Thousand)")
  + scale_x_discrete(limits=top.genres[1:20,]$genre)) %>% print()

# Top average player gain
top.genres <- arrange(top.genres, desc(gain))
(ggplot(data=top.genres[1:20,], aes(x=genre , y=gain)) 
  + geom_bar(stat="identity") 
  + geom_text(aes(label = count),  vjust = 1.5, colour = "white")
  + labs(x="Genre", y="Average Player Gain (Thousand)")
  + scale_x_discrete(limits=top.genres[1:20,]$genre)) %>% print()

# Top average player loss
top.genres <- arrange(top.genres, gain)
(ggplot(data=top.genres[1:20,], aes(x=genre , y=gain)) 
  + geom_bar(stat="identity") 
  + geom_text(aes(label = count),  vjust = 1.5, colour = "black")
  + labs(x="Genre", y="Average Player Gain (Thousand)")
  + scale_x_discrete(limits=top.genres[1:20,]$genre)) %>% print()

# Top average player peak
top.genres <- arrange(top.genres, desc(peak))
(ggplot(data=top.genres[1:20,], aes(x=genre , y=peak)) 
  + geom_bar(stat="identity") 
  + geom_text(aes(label = count),  vjust = 1.5, colour = "white")
  + labs(x="Genre", y="Peak Player Count (Thousand)")
  + scale_x_discrete(limits=top.genres[1:20,]$genre)) %>% print()