#CS-513-A   Kang_Yang   10406720    Movie_Score_Prediction

rm(list = ls())
#Install package 'c50'
install.packages("C50")
require(C50)

#Put the movie data into table
table <-
  read.csv(
    "/Users/Younghong/Downloads/Project/CS513_DataKnockoutProject/movie_metadata.csv"
  )

#Dat preprocessing delete NA data
table_clean <- na.omit(table)

#Get needed variables data
critic = table_clean$num_critic_for_reviews
duration = table_clean$duration
directorFacebookLikes = table_clean$director_facebook_likes
actorFacebookLikes = table_clean$actor_1_facebook_likes
gross = table_clean$gross
votedUsers = table_clean$num_voted_users
castFacebookLikes = table_clean$cast_total_facebook_likes
posterFaces = table_clean$facenumber_in_poster
reviews = table_clean$num_user_for_reviews
country = table_clean$country
budget = table_clean$budget
aspectRatio = table_clean$aspect_ratio
movieFacebookLikes = table_clean$movie_facebook_likes
score = table_clean$imdb_score

#Put data into new table
table_new <-
  data.frame(
    critic,
    duration,
    directorFacebookLikes,
    actorFacebookLikes,
    gross,
    votedUsers,
    castFacebookLikes,
    posterFaces,
    reviews,
    budget,
    aspectRatio,
    movieFacebookLikes,
    score
  )
View(table_new)
str(table_new)
head(table_new)

#Set a seed to random objects that can be reproduced
set.seed(9850)
g <- runif(nrow(table_new))
table_new2 <- table_new[order(g),]
View(table_new2)

#Get test data by every 5 cols
idx = seq(from = 1,
          to = nrow(table_new2),
          by = 5)
#Separate data to test data and training data
test <- table_new2[idx,]
training <- table_new2[-idx,]
training$score <- factor(training$score)

data_pred <- C5.0(training[,-13], training$score)
summary(data_pred)

#C5.0 predicted
p1 <- predict(data_pred, test)
table(test[, 13], C5.0_predicted = p1)

#Put the C5.0 predicted data with test data
results <- cbind(test, p1)
table(results[, 13], results[, 14])

#Only return true when test data and predicted is equal
wrong <- results[, 13] != results[, 14]

#Calculate wrong rate
rate <- sum(wrong) / length(wrong)
rate
sum(wrong)
