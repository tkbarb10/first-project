colnames(star_wars)[33] <- 'Star Trek?'

star_wars <- star_wars |> 
  select(1:36)




train_sample <- star_wars |> 
  slice_sample(prop = .7, replace = T)

test_sample <- star_wars |> 
  slice_sample(prop = .3, replace = T)


# Different way of getting sample -----------------------------------------



sample_index <- sample(2, nrow(star_wars), prob = c(.7, .3), replace = T)

starTrain <- star_wars[sample_index == 1, ]
starTest <- star_wars[sample_index == 2, ]



# Naive Bayes Model -------------------------------------------------------

starModel <- naiveBayes(starTrain$`Star Trek?`~., data = starTrain)

summary(starModel)

starPredict <- predict(starModel, newdata = starTest)

confusionMatrix(starPredict, starTest$`Star Trek?`)

table(starTest$`Star Trek?`)



starModel1 <- naiveBayes(train_sample$`Star Trek?`~., data = train_sample)

summary(starModel1)

starPredict1 <- predict(starModel1, newdata = starTrain)

confusionMatrix(starPredict1, starTrain$`Star Trek?`)
