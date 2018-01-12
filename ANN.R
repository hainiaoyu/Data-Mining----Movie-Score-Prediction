#CS-513-A   Haoyang_Li   10405790    Movie_Score_Prediction

rm(list=ls())

# Install neuralnet package to build the ANN module
install.packages("neuralnet")
install.packages("MASS")
install.packages("grid")
require(neuralnet)
require(MASS)
require(grid)

library(class)

# Read original dataset
table<-read.csv("Documents/movie_metadata.csv")
View(table)

# Data Cleaning to remove null value
table1 <- na.omit(table)
View(table1)

# Extract needed attribution to do the prediction
critic=table1$num_critic_for_reviews
duration=table1$duration
directorFacebookLikes=table1$director_facebook_likes
actorFacebookLikes=table1$actor_1_facebook_likes
gross=table1$gross
votedUsers=table1$num_voted_users
castFacebookLikes=table1$cast_total_facebook_likes
posterFaces=table1$facenumber_in_poster
reviews=table1$num_user_for_reviews
country=table1$country
budget=table1$budget
aspectRatio=table1$aspect_ratio
movieFacebookLikes=table1$movie_facebook_likes
score=table1$imdb_score
table_new<-data.frame(critic, duration, directorFacebookLikes, actorFacebookLikes, gross, votedUsers, castFacebookLikes, posterFaces, reviews, budget, aspectRatio, movieFacebookLikes, score)
View(table_new)
str(table_new)
head(table_new)

# Use max min method to do the data normalization in order to be a caomparable range of 0 to 1
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

# Create a data frame for all the attributes
table_norm <- as.data.frame (         
  cbind(critic = mmnorm(table_new[,1],min(table_new[,1]),max(table_new[,1]))
        ,duration = mmnorm(table_new[,2],min(table_new[,2]),max(table_new[,2]))
        ,directorFacebookLikes = mmnorm(table_new[,3],min(table_new[,3]),max(table_new[,3]))
        ,actorFacebookLikes = mmnorm(table_new[,4],min(table_new[,4]),max(table_new[,4]))
        ,gross = mmnorm(table_new[,5],min(table_new[,5]),max(table_new[,5]))
        ,votedUsers = mmnorm(table_new[,6],min(table_new[,6]),max(table_new[,6]))
        ,castFacebookLikes = mmnorm(table_new[,7],min(table_new[,7]),max(table_new[,7]))
        ,posterFaces = mmnorm(table_new[,8],min(table_new[,8]),max(table_new[,8]))
        ,reviews = mmnorm(table_new[,9],min(table_new[,9]),max(table_new[,9]))
        ,country = mmnorm(table_new[,10],min(table_new[,10]),max(table_new[,10]))
        ,budget = mmnorm(table_new[,11],min(table_new[,11]),max(table_new[,11]))
        ,aspectRatio = mmnorm(table_new[,12],min(table_new[,12]),max(table_new[,12]))
        ,score = mmnorm(table_new[,13],min(table_new[,13]),max(table_new[,13]))
  )
)

head(table_norm)

# Saperate the dataset into training dataset and testing dataset
idx = seq(from = 1,to = nrow(table_norm),by = 5)
test <- table_norm[idx,]
training <- table_norm[-idx,]
View(test)
View(training)

# Build the module with 6 nodes in one hidden layer
nn = neuralnet(score ~ critic + duration + directorFacebookLikes + actorFacebookLikes + gross + 
                 votedUsers + castFacebookLikes + posterFaces + reviews + country + budget + 
                 aspectRatio, data = training, hidden = 6, err.fct = "sse", linear.output = FALSE)
nn
plot(nn)
nn$net.result 
nn$weights
nn$result.matrix

# Calculate the rating score
nn$covariate
training$score
nn$net.result[[1]]

# Calculate the error rate of training
misClassificationError_train = mean(abs(training$score - nn$net.result[[1]]) > 0.1)
misClassificationError_train
OutputVsPred_train = cbind(training$score,nn)
OutputVsPred_train

# Testing the module
new.output = compute(nn, covariate = matrix(as.matrix(test[,-10]), byrow = TRUE, ncol = 12))

# Calculate the error rate of testing
misClassificationError_test = mean(abs(test$score - nn$net.result[[1]]) > 0.1)
misClassificationError_test
OutputVsPred_test=cbind(test$score, nn)
OutputVsPred_test

# Plot the generalized weights of different covoriates and the response variable which is score.
par(mfrow = c(3, 3))
gwplot(nn,selected.covariate = "critic")
gwplot(nn,selected.covariate = "duration")
gwplot(nn,selected.covariate = "directorFacebookLikes")
gwplot(nn,selected.covariate = "actorFacebookLikes")
gwplot(nn,selected.covariate = "gross")
gwplot(nn,selected.covariate = "votedUsers")
gwplot(nn,selected.covariate = "castFacebookLikes")
gwplot(nn,selected.covariate = "posterFaces")
gwplot(nn,selected.covariate = "reviews")
gwplot(nn,selected.covariate = "country")
gwplot(nn,selected.covariate = "budget")
gwplot(nn,selected.covariate = "aspectRatio")









