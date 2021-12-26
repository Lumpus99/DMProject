library(rpart.plot)
library(kernlab)
library(caret)

dt <- rpart(stickiness_avg_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data, cp=0.00085)

rpart.plot(dt, extra=101, main="stickiness_avg.factor")

dt <- rpart(stickiness_sd_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data, cp=0.0008621)

rpart.plot(dt, extra=101, main="stickiness_sd.factor")

dt <- rpart(trendiness_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data, cp=0.0005)


rpart.plot(dt, extra=101, main="trendiness.factor")

dt <- rpart(obsoleteness_factor ~ 
              Indie+Building+Tactical
            +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
            +Singleplayer+Multiplayer+Shooter
            +Survival+Roguelike+Simulation
            +Horror+Strategy+Sports
            +Sandbox, training.data, cp=0.00000000001)

rpart.plot(dt, extra=101, main="obsoleteness.factor")

rndSample <- sample(1:nrow(training.data), 700)

svm.training.data <- training.data[rndSample, ]
svm.testing.data <- training.data[-rndSample, ]

print("stickiness_avg_factor")
s <- ksvm(stickiness_avg_factor ~ 
            Indie+Building+Tactical+Atmospheric+Fantasy+Violent
          +Puzzle+Adventure+Action
          +Singleplayer+Multiplayer, data=svm.training.data, nu = 0.2, epsilon = 0.1, prob.model = FALSE, type = "C-bsvc")

predictions <- predict(s, svm.testing.data) 
#print(table(predictions, svm.testing.data$stickiness_avg_factor, dnn=c("Prediction", "Actual")))
print(confusionMatrix(reference = svm.testing.data$stickiness_avg_factor, data = predictions))


s <- ksvm(stickiness_sd_factor ~ 
            Indie+Building+Tactical+Atmospheric+Fantasy+Violent
          +Puzzle+Adventure+Action
          +Singleplayer+Multiplayer, data=svm.training.data, nu = 0.2, epsilon = 0.1, prob.model = FALSE, type = "C-bsvc")

predictions <- predict(s, svm.testing.data) 
print(confusionMatrix(reference = svm.testing.data$stickiness_sd_factor, data = predictions))

s <- ksvm(trendiness_factor ~ 
            Indie+Building+Tactical
          +Puzzle+Adventure+Action+ Atmospheric+Fantasy+Violent
          +Singleplayer+Multiplayer, data=svm.training.data, nu = 0.2, epsilon = 0.1, prob.model = FALSE, type = "C-bsvc")

predictions <- predict(s, svm.testing.data) 
print(confusionMatrix(reference = svm.testing.data$trendiness_factor, data = predictions))

s <- ksvm(obsoleteness_factor ~ 
            Indie+Building+Tactical
          +Puzzle+Adventure+Action+Atmospheric+Fantasy+Violent
          +Singleplayer+Multiplayer, data=svm.training.data, nu = 0.2, epsilon = 0.1, prob.model = FALSE, type = "C-bsvc")

predictions <- predict(s, svm.testing.data) 
print(confusionMatrix(reference = svm.testing.data$obsoleteness_factor, data = predictions))

#print(plot(predictions))


# print(predictions)
# print(rpart.rules(dt))