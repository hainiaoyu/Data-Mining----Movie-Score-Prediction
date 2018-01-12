###############################################################################
#  Company       : Stevens
#  Course        : CS513A
#  Purpose       : Final project knn algorithm
#  Team remember : XiChen (ID:10406821), HaoYangLi(10405790),KangYang(10406720)

#################################################################################

rm(list = ls())

library(class)

# read the dataset into R
table <- read.csv("/Users/movie_metadata.csv")
table1 <- na.omit(table)

# generate the needed dataset
critic = table1$num_critic_for_reviews
duration = table1$duration
directorFacebookLikes = table1$director_facebook_likes
actorFacebookLikes = table1$actor_1_facebook_likes
gross = table1$gross
votedUsers = table1$num_voted_users
castFacebookLikes = table1$cast_total_facebook_likes
posterFaces = table1$facenumber_in_poster
reviews = table1$num_user_for_reviews
country = table1$country
budget = table1$budget
aspectRatio = table1$aspect_ratio
movieFacebookLikes = table1$movie_facebook_likes
score = table1$imdb_score
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
str(table_new)
head(table_new)

# define max-min normalization function and do normalization
mmnorm <- function(x, minx, maxx) {
  z <- ((x - minx) / (maxx - minx))
  return(z)
}

table_norm <- as.data.frame (
  cbind(
    critic = mmnorm(table_new[, 1], min(table_new[, 1]), max(table_new[, 1]))
    ,
    duration = mmnorm(table_new[, 2], min(table_new[, 2]), max(table_new[, 2]))
    ,
    directorFacebookLikes = mmnorm(table_new[, 3], min(table_new[, 3]), max(table_new[, 3]))
    ,
    actorFacebookLikes = mmnorm(table_new[, 4], min(table_new[, 4]), max(table_new[, 4]))
    ,
    gross = mmnorm(table_new[, 5], min(table_new[, 5]), max(table_new[, 5]))
    ,
    votedUsers = mmnorm(table_new[, 6], min(table_new[, 6]), max(table_new[, 6]))
    ,
    castFacebookLikes = mmnorm(table_new[, 7], min(table_new[, 7]), max(table_new[, 7]))
    ,
    posterFaces = mmnorm(table_new[, 8], min(table_new[, 8]), max(table_new[, 8]))
    ,
    reviews = mmnorm(table_new[, 9], min(table_new[, 9]), max(table_new[, 9]))
    ,
    country = mmnorm(table_new[, 10], min(table_new[, 10]), max(table_new[, 10]))
    ,
    budget = mmnorm(table_new[, 11], min(table_new[, 11]), max(table_new[, 11]))
    ,
    aspectRatio = mmnorm(table_new[, 12], min(table_new[, 12]), max(table_new[, 12]))
    ,
    score = mmnorm(table_new[, 13], min(table_new[, 13]), max(table_new[, 13]))
  )
)

head(table_norm)

# generate training dataset and test dataset
idx = seq(from = 1,
          to = nrow(table_norm),
          by = 5)
test <- table_norm[idx, ]
training <- table_norm[-idx, ]
View(test)
View(training)

# use knn algorithm to build the model
predict <- knn(training[, -13], test[, -13], training[, 13], k = 1)

# combine the prediction with the test data and calculate the wrong rate
results <- cbind(test, predict)
table(results[, 13], results[, 14])
wrong <- results[, 13] != results[, 14]
rate <- sum(wrong) / length(wrong)
rate
