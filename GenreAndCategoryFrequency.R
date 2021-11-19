library(tidyverse)
library(ggplot2)
# https://stackoverflow.com/questions/48350991/filter-a-dataframe-by-values-in-a-column-of-type-list
# survival.games <- filter(steam.games, map_lgl(steam.games$popu_tags, ~'Survival' %in% .))
# steam.merged.games.charts <- merge(x = survival.games, y = steam.charts, by = "name")
# table(unlist(steam.genre.data))

generate.most.frequent <- function(list.param, x, y){
  df <- (list.param 
         %>% summary() 
         %>% sort(decreasing=TRUE) 
         %>% as.data.frame())
  
  df <- (df 
         %>% rownames() 
         %>% cbind(df))
  
  rownames(df) <- NULL
  colnames(df) <- c(x,y)
  df
}

genre.summary.data <- generate.most.frequent(steam.all.genres,"Genre","Count")
category.summary.data <- generate.most.frequent(steam.all.categories,"Category","Count")


(ggplot(data=genre.summary.data[2:21,], aes(x=Genre , y=Count)) 
+ geom_bar(stat="identity") 
+ scale_x_discrete(limits=genre.summary.data[2:21,]$Genre)) %>% print()

(ggplot(data=category.summary.data[2:11,], aes(x=Category , y=Count)) 
  + geom_bar(stat="identity") 
  + scale_x_discrete(limits=category.summary.data[2:11,]$Category)) %>% print()