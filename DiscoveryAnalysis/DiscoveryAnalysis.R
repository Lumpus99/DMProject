library(rpart.plot)
library(kernlab)
library(caret)

dt <- rpart(stickiness_avg_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data.factor, cp=0.0035)

rpart.plot(dt, extra=101, main="stickiness_avg")

dt <- rpart(stickiness_sd_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data.factor, cp=0.003621)

rpart.plot(dt, extra=101, main="stickiness_sd")

dt <- rpart(trendiness_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data.factor, cp=0.005)


rpart.plot(dt, extra=101, main="trendiness")

dt <- rpart(obsoleteness_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data.factor, cp=0.003)

rpart.plot(dt, extra=101, main="obsoleteness")

rndSample <- sample(1:nrow(training.data.factor), 700)

svm.training.data <- training.data.factor[rndSample, ]
svm.testing.data <- training.data.factor[-rndSample, ]

s <- ksvm(stickiness_avg_factor ~ 
            Indie+Building+Tactical+Atmospheric+Fantasy+Violent
          +Puzzle+Adventure+Action
          +Singleplayer+Multiplayer, data=svm.training.data)

predictions <- predict(s, svm.testing.data) 
print(table(predictions, svm.testing.data$stickiness_avg_factor, dnn=c("Prediction", "Actual")))
print(confusionMatrix(reference = svm.testing.data$stickiness_avg_factor, data = predictions))


s <- ksvm(stickiness_sd_factor ~ 
            Indie+Building+Tactical+Atmospheric+Fantasy+Violent
          +Puzzle+Adventure+Action
          +Singleplayer+Multiplayer, data=svm.training.data)

predictions <- predict(s, svm.testing.data) 
print(confusionMatrix(reference = svm.testing.data$stickiness_sd_factor, data = predictions))

s <- ksvm(trendiness_factor ~ 
            Indie+Building+Tactical
          +Puzzle+Adventure+Action+ Atmospheric+Fantasy+Violent
          +Singleplayer+Multiplayer, data=svm.training.data)

predictions <- predict(s, svm.testing.data) 
print(confusionMatrix(reference = svm.testing.data$trendiness_factor, data = predictions))

s <- ksvm(obsoleteness_factor ~ 
            Indie+Building+Tactical
          +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
          +Singleplayer+Multiplayer, data=svm.training.data)

predictions <- predict(s, svm.testing.data) 
print(confusionMatrix(reference = svm.testing.data$obsoleteness_factor, data = predictions))

#print(plot(predictions))


# print(predictions)
# print(rpart.rules(dt))