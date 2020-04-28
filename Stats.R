library(ISLR)
library(tidyverse)
library(caret)
library(MASS)
library(MLeval)
library(leaps)
myurl  <- "https://raw.githubusercontent.com/reisanar/datasets/master/Airfares.csv"
Airfare <- read_csv(myurl)

head(Airfare)

Airfare <- na.omit(Airfare)

Num.Airfare <- Airfare %>% select_if(is.numeric)

head(Num.Airfare)

Num.Airfare.cor <- round(cor(Num.Airfare), 3)

Num.Airfare.cor

pairs(Num.Airfare)

new.Airfare <- Airfare[c(5:18)]

head(new.Airfare)

new.Airfare$VACATION <- Airfare$VACATION

new.Airfare$SW <- Airfare$SW

new.Airfare <- new.Airfare %>% mutate(VACATION = ifelse(VACATION == "No" ,0,1))

new.Airfare <- new.Airfare %>% mutate(SW = ifelse(SW == "No" ,0,1))

new.Airfare$SLOT <- recode(new.Airfare$SLOT, Free = "NO", Controlled = "YES")

new.Airfare <- new.Airfare %>% mutate(SLOT = ifelse(SLOT == "NO" ,0,1))

new.Airfare$GATE <- recode(new.Airfare$GATE, Free = "NO", Controlled = "YES")

new.Airfare <- new.Airfare %>% mutate(GATE = ifelse(GATE == "NO" ,0,1))

head(new.Airfare)

new.Airfare.cor <- round(cor(new.Airfare), 3)

new.Airfare.cor

train_index <- sample(1:nrow(new.Airfare), 0.8 * nrow(new.Airfare))
test_index <- setdiff(1:nrow(new.Airfare), train_index)

X_train <- new.Airfare[train_index, 1:13]
y_train <- new.Airfare[train_index, 14]

full.model <- lm(FARE ~., data =new.Airfare )

step.model <- stepAIC(full.model, direction = "both", trace = FALSE)
summary(step.model)


train.control <- trainControl(method = "cv", number = 10)
step.model <- train(FARE ~., data = new.Airfare, method = "leapSeq", tuneGrid = data.frame(nvmax = 1:5), trControl = train.control)
step.model$results


step.model$bestTune

best.summary <-summary(step.model$finalModel)
best.summary

plot(best.summary$cp, xlab = "number of features", ylab = "cp")
plot(step.model$finalModel, scale = "Cp")
coef(step.model$finalModel, 5)

