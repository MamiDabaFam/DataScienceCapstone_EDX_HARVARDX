## MAMI DABA FAM- SCRIPT IN R- MovieLens Project Submission ##
## Datascience capstone- EDX ###



##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
# Download of movielens data failed suddenly, I have completed the download file options

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl, 
              quiet = TRUE, mode = "wb", method = "auto", cacheOK = TRUE, 
              options(timeout = max(1000, getOption("timeout"))))

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

##############################################################################
# edx DATA Exploration 
##############################################################################

#structure of edx
str(edx)

dim(edx)

#Number of movies in edx data
n_distinct(edx$movieId)

#Number of users in edx data
n_distinct(edx$userId)

# Number of movies that did not have a rating
sum(edx$rating == 0)

#How many rating categories are included in the data set 
n_distinct(edx$rating)

#distribution of the rating
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()

## Users- movies effects analysis ##

## users- movies matrix by rating 

# Sample of first 100 movies and users 
edx%>% group_by(rating) %>% filter(userId<= 100 & movieId <= 100)%>% 
  summarise(n=n(), movieId = movieId, userId =userId) %>% 
  ggplot(aes(x=movieId, y= userId, color = 'rating')) +
  geom_point()+ ggtitle('Sample 1 : User- Movie Matrix')

# Sample of 100 users and movies with ID between 900 and 1000

edx%>% group_by(rating) %>% 
  filter(userId<= 1000 & userId>= 900 & movieId <= 1000 & movieId>= 900)%>% 
  summarise(n=n(), movieId = movieId, userId =userId) %>% 
  ggplot(aes(x=movieId, y= userId, color = 'rating'))+
  geom_point()+ ggtitle('Sample 2 : User- Movie Matrix')

# User's rating behavior
edx %>% group_by(userId) %>% summarise(n =n()) %>% 
  ggplot(aes(n))+geom_histogram(color='blue')+scale_x_log10()+ 
  ggtitle('Users rating behavior')

# Movie's rating behavior 
edx %>% group_by(movieId) %>% summarise(n =n()) %>% ggplot(aes(n))+
  geom_histogram(color='red')+scale_x_log10()+ggtitle('movies rating behavior')

## Rating by movie's genres 

edx %>% group_by (genres) %>% summarise(n=n()) 

edx %>% group_by (genres) %>% summarise(n=n())%>% arrange(desc(n))

# Number of ratings for Drama movie genres
drama_rating <-edx %>% group_by (genres) %>% summarise(n=n()) %>%
  filter(grepl('Drama', genres))

sum(drama_rating$n)


## Split edx data into training set and 20% test set for models optimization

set.seed(1, sample.kind = "Rounding")
edx_test_index <- createDataPartition(y= edx$rating, times = 1, p= 0.2, list = FALSE)
edx_training <- edx[- edx_test_index, ]
temp_edx <- edx[edx_test_index, ]

#Make sure userId and movieId in test set are also in training set

edx_test <- temp_edx  %>% semi_join(edx_training, by = "movieId") %>% 
  semi_join(edx_training, by = "userId")

# Add rows removed from test set back into training set 

 removed_edx <- anti_join(temp_edx, edx_test)
 edx_training <- rbind(edx_training, removed_edx)
rm( edx_test_index, temp_edx, removed_edx)

### Global effects on our rating outcome analysis #################

# Average of ratings in edx training data
mu_hat <- mean(edx_training$rating)

## movies effects on  rating

## b_i is the effect of each movie on rating.
## We know that some movies are most popular than others
## They are able to get more ratings/ higher ratings then others


movie_effect <- edx_training %>% group_by(movieId)%>% 
  summarize(b_i = mean(rating- mu_hat), rating = mean(rating))

movie_effect %>% ggplot(aes(movieId, rating))+ geom_point( color = 'blue')

# Plot the bias b_i for movie i 
movie_effect %>% ggplot(aes(b_i)) + geom_histogram(bins = 30, color = "black")



## User's effects on rating 

## b_u is the effect of each user on rating.
## We know that some users tend to rate movies lower than the average user.

user_effect <- edx_training %>% group_by(userId)%>% 
  summarize( b_u = mean(rating- mu_hat),rating = mean(rating))

user_effect %>% ggplot(aes(userId, rating)) + geom_point()

# Plot the user specific effect b_u 
user_effect %>% ggplot(aes(b_u))+ geom_histogram(bins = 30, color = "orange")
 

##  Date effect's on rating 

# Install lubridate packages 
library(lubridate)

#Transform our timestamp as a datetime
edx_training <- mutate( edx_training, date = as_datetime(timestamp))

date_effect <- edx_training %>% mutate(date = round_date(date, unit = "week"))%>%
  group_by(date) %>% summarize(b_d = mean(rating- mu_hat),rating = mean(rating))

date_effect %>% ggplot(aes(date, rating)) + geom_point() + geom_smooth()

# Plot date specific effect on rating b_d
date_effect %>% ggplot(aes(b_d)) + geom_histogram(bins = 30, color = "Blue")

## Genres effects on rating less then 150 movies

edx_training %>% group_by(genres) %>% 
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n < 150) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##############################################################################
### Linear Regression Models
##############################################################################

# As mentioned in the report, the RMSE (Root Mean Squared Error) will be use to evaluate our several linear models.

# RMSE can be calculated by creating a function.
# I make the choice to work with the rmse() function in the metrics library
#load metrics library

library(Metrics)


##### MODEL 1 - NAIVE #######################################################

naive_rmse <- rmse(edx_test$rating, mu_hat)

##### Model 2 : considering the movie effect ##################################

movie_effect <- edx_training %>% group_by(movieId)%>% 
  summarize(b_i = mean(rating- mu_hat))

# prediction of rating considering movie effect
pred_movie <- mu_hat + edx_test %>% left_join(movie_effect, by='movieId') %>% .$b_i

#RMSE of the model 2 taking account the movie effect
movie_rmse <- rmse(edx_test$rating, pred_movie)

#### Model 3 : As we now that adding extra predictors- can improve our RMSE.
# We will add the user effects in our Model

#Let's first report our movie effect
movie_effect <- edx_training %>% group_by(movieId)%>% summarize(b_i = mean(rating- mu_hat))

# Include now user's effect with movie effect
user_effect <- edx_training %>% left_join(movie_effect, by = "movieId") %>%
  group_by(userId)%>% summarize( b_u = mean(rating- b_i- mu_hat))

# prediction of rating considering movie and user effects
pred_movie_user <- edx_test %>% left_join(movie_effect, by='movieId') %>%
  left_join(user_effect, by = 'userId')%>% mutate(pred = mu_hat +b_i + b_u)%>% pull(pred)

#RMSE of the model 3 taking account the movie and user effects
movie_user_rmse <- rmse(pred_movie_user, edx_test$rating)

#### Model 4 : regularization of movies- users effects 

lambdas <- seq(0,10, 0.25)

## Calculate RMSEs of all values of lambda for the regularized user/movie effects
lambda_rmses <- sapply(lambdas, function(l) {
  mu_hat <- mean(edx_training$rating)
  
  #Regularized movies effect
  movie_effect_r <- edx_training %>% group_by(movieId)%>% 
    summarize(b_i_r = sum(rating- mu_hat)/(n()+l))
  
  #Regularized users effect
  user_effect_r <- edx_training %>% left_join(movie_effect_r, by= 'movieId')%>%
    group_by(userId)%>% summarize( b_u_r = sum(rating - b_i_r - mu_hat)/(n()+l))
  
  # prediction of rating considering regularized movie and user effects
  pred_movie_user_reg <- mu_hat + (edx_test %>% 
                                     left_join(movie_effect_r, by='movieId') %>% 
                                     left_join(user_effect_r, by = 'userId')%>% 
                                     mutate(bi_u_r =b_i_r + b_u_r)%>% 
                                     pull(bi_u_r))
  #Compute RMSEs for each lambda
  return( rmse(pred_movie_user_reg, edx_test$rating) )
})

# Compute lambda that minimizes the RMSE of regularized movie-user effects 
lambda <- lambdas[which.min(lambda_rmses)]

### Model 5 : Regularization of movie and user effect with the best lambda that minimizes the RMSE

# Movie effect regularized with lambda
movie_effect_l <- edx_training %>% group_by(movieId)%>% 
  summarize(b_i_l = sum(rating- mu_hat)/(n()+lambda))

#User and movie effects regularized with lambda 
user_effect_l <- edx_training %>% left_join(movie_effect_l, by= 'movieId')%>% 
  group_by(userId)%>% summarize( b_u_l = sum(rating- b_i_l - mu_hat)/(n()+lambda))

# prediction of rating considering regularized movie and user effects
pred_movie_user_l <- mu_hat + (edx_test %>% left_join(movie_effect_l, by='movieId') %>% 
                                 left_join(user_effect_l, by = 'userId')%>% 
                                 mutate(bi_u_l =b_i_l + b_u_l)%>% pull(bi_u_l))

#RMSE of the model 5 taking account the best lambda of regularized movie and user effects
movie_user_lambda_rmse <- rmse(pred_movie_user_l, edx_test$rating)

###############################################################################################
# VALIDATION SET- WITH MODEL 5 REGULARIZED OF MOVIE- USER EFFECT WITH THE BEST LAMBDA OF 4.75##
###############################################################################################

# Average of rating in the edx data set
mu <- mean(edx$rating)

# The final movie effect calculated on edx  and regularized using lambda
movie_effect_final <- edx %>% group_by(movieId) %>% summarize(b_i_f = sum(rating- mu)/(n()+ lambda))

# The final user and movie effects calculated on edx  and regularized using lambda
user_effect_final <- edx %>% left_join(movie_effect_final, by= 'movieId')%>% 
  group_by(userId)%>% summarize( b_u_f = sum(rating- b_i_f - mu)/(n()+lambda))

# Final prediction of rating considering regularized movie and user effects
prediction_rating <- mu + (validation %>% left_join(movie_effect_final, by='movieId') %>% 
                             left_join(user_effect_final, by = 'userId')%>% mutate(bi_u_f =b_i_f + b_u_f)%>% pull(bi_u_f))

# compute final RMSE of my project 
Final_rmse <- rmse(prediction_rating, validation$rating)

## Test and highlight the best 20 movies

validation%>% left_join(movie_effect_final, by='movieId') %>% 
  left_join(user_effect_final, by = 'userId')%>% mutate(pred =mu+ b_i_f + b_u_f)%>% 
  arrange(-pred)%>% group_by(title)%>% select(title, genres)%>% head(20)


## Highlight the 20 worst rated movies 

validation%>% left_join(movie_effect_final, by='movieId') %>% 
  left_join(user_effect_final, by = 'userId')%>% mutate(pred =mu+ b_i_f + b_u_f)%>% 
  arrange(desc(-pred))%>% group_by(title)%>% select(title, genres)%>% head(20)


