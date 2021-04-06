##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
head(movies)

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
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

###############################
# Project
##############################
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(stringr)

head(validation)
head(edx)
str(edx)

### data prep, Manipulation ###

# edit timestamp
edx <- edx %>% mutate(timestamp = as_datetime(timestamp)) %>% 
  mutate(userId = as.factor(userId), movieId = as.factor(movieId))

# seperate movie release year
edx <- edx %>% mutate(movieYear = str_sub(title, -5,-2)) %>%
  mutate(movieYear = as.numeric(movieYear))

# seperate rating year, month
edx <-  edx %>% mutate(timestamp = as_datetime(timestamp)) %>%
  mutate(ratingYear = year(timestamp), ratingMonth = month(timestamp))

# get number of genres
edx <- edx %>% mutate(numberGenres = (str_count(genres, pattern = "\\|")+1))

### EDA 
## UserId

edx %>% ggplot(aes(x = rating)) + geom_histogram(bins = 10) + 
  labs(title = 'Rating Distribution')
edx %>% group_by(userId) %>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(x = meanRating)) + geom_histogram(bins = 10)  + 
  labs(title = 'Mean Rating Distribution by UserId')

edx %>% ggplot(aes(y = rating)) + geom_boxplot() + 
  labs(title = 'Rating Distribution')
edx %>% group_by(userId) %>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(y = meanRating)) + geom_boxplot()  + 
  labs(title = 'Mean Rating Distribution by UserId')
# Some user rated very low in the same time some may vote very high #
# there is an outlier on rating of each movie, this shows some bias from each user #

#top 5 rate-count userId
edx %>% group_by(userId) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% top_n(n = 5) %>%
  ggplot(aes(x = userId, y = count)) + geom_col() +
  labs(title = 'Top 5 Rate-Count User')

edx %>% filter(userId %in% c(59269,67385,14463,68259,27468)) %>% 
  ggplot(aes(y = rating, x = userId)) + geom_boxplot() +
  labs(title = 'Rating Distribution of Top 5 Rate-Count User')

# Try to see any difference average rating between users with difference numbers of() rating #
edx %>% group_by(userId) %>% summarize(count = n()) %>%
  ggplot(aes(x = count)) + geom_histogram(bins = 300) + 
  labs(title = 'Number-of-Rating Distribution by User') + 
  xlab('Number of Rating')

# There are some users that have rated a lot #
edx %>% group_by(userId) %>% summarize(count = n()) %>% 
  summarize(quantile = quantile(count,probs =c(0.95)))
edx %>%group_by(userId) %>% summarize(count = n(), meanRating = mean(rating)) %>% 
  mutate(Q95 = (count>461)) %>%
  ggplot(aes(y = meanRating, x = Q95)) + geom_boxplot() + 
  xlab('If number of rating more than 461 (95% of total user)')
# user with more rating tends to have a lower average rating score #
# May need to do regularization on UserId #

## MovieId

# let's see if the movie with more rating have any difference in mean rating or not
edx %>% ggplot(aes(x = rating)) + geom_histogram(bins = 10) + 
  labs(title = 'Rating Distribution')
edx %>% group_by(movieId) %>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(x = meanRating)) + geom_histogram(bins = 10) + 
  labs(title = 'Mean Rating Distribution by MovieId')

edx %>% ggplot(aes(y = rating)) + geom_boxplot() + 
  labs(title = 'Rating Distribution')
edx %>% group_by(movieId) %>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(y = meanRating)) + geom_boxplot() + 
  labs(title = 'Mean Rating Distribution by MovieId')
# This shows bias from movieId

# top 5 rated movies
edx %>% group_by(movieId, title) %>% summarize(count = n()) %>% 
  arrange(desc(count)) %>% head(n = 5) %>%
  ggplot(aes(x = title, y = count)) + geom_col() +
  scale_x_discrete(guide = guide_axis(angle = 30)) + 
  labs(title = 'Top 5 Rate-Count Movie ')

edx %>% filter(movieId %in% c(296,356,593,480,318)) %>% 
  ggplot(aes(y = rating, x = title)) + geom_boxplot() +
  scale_x_discrete(guide = guide_axis(angle = 30)) + 
  labs(title = 'Rating Distribution of Top 5 Rate-Count Movie ')

# difference in number of rating may effect, let's see #
edx %>% group_by(movieId) %>% summarize(count = n()) %>%
  ggplot(aes(x = count)) + geom_histogram(bins = 50) + 
  labs(title = 'Number-of-Rating Distribution by User') + 
  xlab('Number of Rating')
# some movie have alot more ratings than others #

# Let's see if there is any difference in number of rating may effect
edx %>% group_by(movieId) %>% summarize(count = n()) %>% 
  summarize(quantile = quantile(count,probs =c(0.95)))
edx %>%group_by(movieId) %>% summarize(count = n(), meanRating = mean(rating)) %>% 
  mutate(Q95 = (count>4026)) %>%
  ggplot(aes(y = meanRating, x = Q95)) + 
  geom_boxplot() +
  labs(title = 'Mean rating of Higher Rate-count Movie and Lower Rate-count Movie') +
  xlab('If the movie have higer number of rating (95% of total movie)')
# Movie with more rating tends to have higher average rating
# Regularization may need on movieId

## Movie Release Year
edx %>% group_by(movieYear) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(x = movieYear, y = meanRating, group = 1)) + geom_line() + 
  labs(title = 'Average Rating by Year of Release')

edx %>% ggplot(aes(x = rating)) + geom_histogram(bins = 10) + 
  labs(title = 'Rating Distribution')
edx %>% group_by(movieYear) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(x = meanRating)) + geom_histogram(bins = 10) +
  scale_x_continuous(limits=c(0,5)) + 
  labs(title = 'Mean Rating Distribution by Release Year')

edx %>% ggplot(aes(y = rating)) + geom_boxplot() + 
  labs(title = 'Rating Distribution')
edx %>% group_by(movieYear) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(y = meanRating)) + geom_boxplot() + 
  labs(title = 'Mean Rating Distribution by Release Year')
# the average rating on each release year has a different, this shows bias on movie release year
# the average rating on each release year has no different, this shows no bias on movie release year

## Movie Rating Year

edx %>% group_by(ratingYear) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(x = ratingYear, y = meanRating, group = 1)) + geom_line() + 
  labs(title = 'Average Rating by Rating Year')

edx %>% group_by(ratingYear)%>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(x = meanRating)) + geom_histogram(bins = 10) +
  scale_x_continuous(limits=c(0,5)) + 
  labs(title = 'Mean Rating Distribution by Rating Year')

# Movie Rating Month
edx %>% group_by(ratingMonth) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(x = ratingMonth, y = meanRating, group = 1)) + geom_line() + 
  labs(title = 'Average Rating by Rating Month')

edx %>% group_by(ratingMonth)%>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(x = meanRating)) + geom_histogram(bins = 10) +
  scale_x_continuous(limits=c(0,5)) + 
  labs(title = 'Mean Rating Distribution by Rating Year Month')


# Number of Genres
edx %>% group_by(numberGenres) %>% summarize(meanRating = mean(rating)) %>% 
  ggplot(aes(x = numberGenres, y = meanRating, group = 1)) + geom_line()  + 
  labs(title = 'Average Rating by Number of Genres')

edx %>% group_by(numberGenres)%>% summarize(meanRating = mean(rating)) %>%
  ggplot(aes(x = meanRating)) + geom_histogram(bins = 10) +
  scale_x_continuous(limits=c(0,5)) + 
  labs(title = 'Mean Rating Distribution by Number of Genres')

### Model Fitting ###
## Train, Test split ##
set.seed(1, sample.kind="Rounding") 
trainIndex <- createDataPartition(edx$rating, times = 1, p = 0.8, list = FALSE)
trainSet <- edx[trainIndex,]
testSet <- edx[-trainIndex,]

testSet <- testSet %>% semi_join(trainSet, by = 'userId') %>%
  semi_join(trainSet, by = 'movieId')

## RMSE Function ##
RMSE <- function(real , pred) {
  sqrt(mean((real - pred)^2))
}

## Predict by Mean ##
mu_rating = mean(trainSet$rating)

rmse_mu_rating <- RMSE(testSet$rating, mu_rating)
rmse_mu_rating

result <- data_frame(method = 'Mean', RMSE = rmse_mu_rating)
result %>% knitr::kable()

## Predict by Mean + User ##
user_rating <- trainSet %>% group_by(userId) %>% 
  summarize(b_u = mean(rating - mu_rating))

Pred_RatingUser <- testSet %>% left_join(user_rating, by = 'userId') %>% 
  mutate(Pred = mu_rating + b_u) %>% pull(Pred)
Pred_RatingUser

rmseRatingUser <- RMSE(testSet$rating, Pred_RatingUser)
rmseRatingUser

result <- bind_rows(result, data_frame(method = 'User_bias', RMSE = rmseRatingUser))
result %>% knitr::kable()

## Predict by Mean + User + Movie ##

user_movie_rating <- trainSet %>% left_join(user_rating, by = 'userId') %>%
  group_by(movieId) %>% summarize(b_u_m = mean(rating - mu_rating - b_u))

Pred_RatingUserMovie <- testSet %>% left_join(user_rating, by = 'userId') %>%
  left_join(user_movie_rating, by = 'movieId') %>%
  mutate(Pred = mu_rating + b_u + b_u_m) %>% pull(Pred)
Pred_RatingUserMovie

rmseRatingUserMovie <- RMSE(testSet$rating, Pred_RatingUserMovie)
rmseRatingUserMovie

result <- bind_rows(result, data_frame(method = 'User_Movie_bias', RMSE = rmseRatingUserMovie))
result %>% knitr::kable()

## Predict by Mean + Movie ##
movie_rating <- trainSet %>% group_by(movieId) %>% 
  summarize(b_m = mean(rating - mu_rating))

Pred_RatingMovie <- testSet %>% inner_join(movie_rating, by = 'movieId') %>%
  mutate(Pred = mu_rating + b_m) %>% pull(Pred)

rmseRatingMovie <- RMSE(testSet$rating, Pred_RatingMovie)
rmseRatingMovie

result <- bind_rows(result, data_frame(method = 'Movie_bias', RMSE = rmseRatingMovie))
result %>% knitr::kable()

## Predict by Mean + Movie + User##

movie_user_rating <- trainSet %>% left_join(movie_rating, by = 'movieId') %>%
  group_by(userId) %>% summarize(b_m_u = mean(rating - mu_rating - b_m))

Pred_RatingMovieUser <- testSet %>% left_join(movie_rating, by = 'movieId') %>%
  left_join(movie_user_rating, by = 'userId') %>%
  mutate(Pred = mu_rating + b_m + b_m_u) %>% pull(Pred)

rmseRatingMovieUser <- RMSE(testSet$rating, Pred_RatingMovieUser)
rmseRatingMovieUser

result <- bind_rows(result, data_frame(method = 'Movie_User_bias', RMSE = rmseRatingMovieUser))
result %>% knitr::kable()

## Predict by Mean + MovieYear ##
head(trainSet)
movieYear_rating <- trainSet %>% group_by(movieYear) %>% 
  summarize(b_my = mean(rating - mu_rating))

Pred_RatingMovieYear <- testSet %>% 
  inner_join(movieYear_rating, by = 'movieYear') %>%
  mutate(Pred = mu_rating + b_my) %>% pull(Pred)

rmseRatingMovieYear <- RMSE(testSet$rating, Pred_RatingMovieYear)
rmseRatingMovieYear

result <- bind_rows(result, data_frame(method = 'MovieYear_bias', RMSE = rmseRatingMovieYear))
result %>% knitr::kable()

## Predict by Mean + Movie + User + MovieYear ##

movie_user_movieYear_rating <- trainSet %>% left_join(movie_rating, by = 'movieId') %>%
  left_join(movie_user_rating, by = 'userId') %>%
  group_by(movieYear) %>% summarize(b_m_u_my = mean(rating - mu_rating - b_m - b_m_u))

Pred_RatingMovieUserMovieYear <- testSet %>% left_join(movie_rating, by = 'movieId') %>%
  left_join(movie_user_rating, by = 'userId') %>% 
  left_join(movie_user_movieYear_rating, by = 'movieYear') %>%
  mutate(Pred = mu_rating + b_m + b_m_u + b_m_u_my) %>% pull(Pred)

rmseRatingMovieUserMovieYear <- RMSE(testSet$rating, Pred_RatingMovieUserMovieYear)
rmseRatingMovieUserMovieYear

result <- bind_rows(result, data_frame(method = 'Movie_User_MovieYear_bias', 
                                       RMSE = rmseRatingMovieUserMovieYear))
result %>% knitr::kable()

## Regularization ##
lambdas <- seq(1,10,0.25)

rmses <- sapply(lambdas, function(l){
  movie_reg_rating <- trainSet %>% group_by(movieId) %>% 
    summarize(b_m_reg = sum(rating - mu_rating)/(n() + l))

  movie_user_reg_rating <- trainSet %>% left_join(movie_reg_rating, by = 'movieId') %>%
    group_by(userId) %>% summarize(b_m_u_reg = sum(rating - mu_rating - b_m_reg)/(n() + l))

  movie_user_movieYear_reg_rating <- trainSet %>% left_join(movie_reg_rating, by = 'movieId') %>%
    left_join(movie_user_reg_rating, by = 'userId') %>%
    group_by(movieYear) %>% summarize(b_m_u_my_reg = sum(rating - mu_rating - b_m_reg - b_m_u_reg)/(n() + l))

  Pred_RatingMovieUserMovieYearReg <- testSet %>% left_join(movie_reg_rating, by = 'movieId') %>%
    left_join(movie_user_reg_rating, by = 'userId') %>% 
    left_join(movie_user_movieYear_reg_rating, by = 'movieYear') %>%
    mutate(Pred = mu_rating + b_m_reg + b_m_u_reg + b_m_u_my_reg) %>% pull(Pred)
  
  Pred_RatingMovieUserMovieYearReg
  Pred_RatingMovieUserMovieYearRegCort <- if_else(Pred_RatingMovieUserMovieYearReg < 0, 0,Pred_RatingMovieUserMovieYearReg)
  Pred_RatingMovieUserMovieYearRegCort <- if_else(Pred_RatingMovieUserMovieYearRegCort > 5, 5, Pred_RatingMovieUserMovieYearRegCort)
  
  rmseRatingMovieUserMovieYearReg <- RMSE(testSet$rating, Pred_RatingMovieUserMovieYearRegCort)
  rmseRatingMovieUserMovieYearReg
})

## Plot lambda ##
regtable <- data_frame(no = seq(1:length(rmses)),seq = seq(1,10,0.25), RMSE = rmses) %>% knitr::kable()
regtable

qplot(seq(1,10,0.25),rmses)
which.min(rmses)

reg <- data_frame(no = seq(1:length(rmses)),seq = seq(1,10,0.25), RMSE = rmses)
reg
### Minimum RMSE, Regularization Degree ###
reg[which.min(rmses),]

## Regularization Application Code##
l = reg[[which.min(rmses),'seq']]
movie_reg_rating <- trainSet %>% group_by(movieId) %>% 
  summarize(b_m_reg = sum(rating - mu_rating)/(n() + l))

movie_user_reg_rating <- trainSet %>% left_join(movie_reg_rating, by = 'movieId') %>%
  group_by(userId) %>% summarize(b_m_u_reg = sum(rating - mu_rating - b_m_reg)/(n() + l))

movie_user_movieYear_reg_rating <- trainSet %>% left_join(movie_reg_rating, by = 'movieId') %>%
  left_join(movie_user_reg_rating, by = 'userId') %>%
  group_by(movieYear) %>% summarize(b_m_u_my_reg = sum(rating - mu_rating - b_m_reg - b_m_u_reg)/(n() + l))

Pred_RatingMovieUserMovieYearReg <- testSet %>% left_join(movie_reg_rating, by = 'movieId') %>%
  left_join(movie_user_reg_rating, by = 'userId') %>% 
  left_join(movie_user_movieYear_reg_rating, by = 'movieYear') %>%
  mutate(Pred = mu_rating + b_m_reg + b_m_u_reg + b_m_u_my_reg) %>% pull(Pred)

rmseRatingMovieUserMovieYearReg <- RMSE(testSet$rating, Pred_RatingMovieUserMovieYearReg)
rmseRatingMovieUserMovieYearReg

result <- bind_rows(result, data_frame(method = 'Movie_User_MovieYear_bias_Reg', 
                                       RMSE = rmseRatingMovieUserMovieYearReg))
result %>% knitr::kable()

### Correction RMSE<0, RMSE>5
Pred_RatingMovieUserMovieYearReg
Pred_RatingMovieUserMovieYearRegCort <- if_else(Pred_RatingMovieUserMovieYearReg < 0, 0,Pred_RatingMovieUserMovieYearReg)
Pred_RatingMovieUserMovieYearRegCort <- if_else(Pred_RatingMovieUserMovieYearRegCort > 5, 5, Pred_RatingMovieUserMovieYearRegCort)

rmseRatingMovieUserMovieYearRegCort <- RMSE(testSet$rating, Pred_RatingMovieUserMovieYearRegCort)
rmseRatingMovieUserMovieYearRegCort

result <- bind_rows(result, data_frame(method = 'Movie_User_MovieYear_bias_Reg_Cort', 
                                       RMSE = rmseRatingMovieUserMovieYearRegCort))
result %>% knitr::kable()

### Validation

# edit timestamp
EDXvalidation <- validation %>% mutate(timestamp = as_datetime(timestamp)) %>% 
  mutate(userId = as.factor(userId), movieId = as.factor(movieId))

# seperate movie release year
EDXvalidation <- EDXvalidation %>% mutate(movieYear = str_sub(title, -5,-2)) %>%
  mutate(movieYear = as.numeric(movieYear))

str(EDXvalidation)

# regularization
EDXlambdas <- seq(1,10,0.25)

EDXrmses <- sapply(EDXlambdas, function(l){
  edx_movie_reg_rating <- edx %>% group_by(movieId) %>% 
    summarize(b_m_reg = sum(rating - mu_rating)/(n() + l))
  
  edx_movie_user_reg_rating <- edx %>% left_join(edx_movie_reg_rating, by = 'movieId') %>%
    group_by(userId) %>% summarize(b_m_u_reg = sum(rating - mu_rating - b_m_reg)/(n() + l))
  
  edx_movie_user_movieYear_reg_rating <- edx %>% left_join(edx_movie_reg_rating, by = 'movieId') %>%
    left_join(edx_movie_user_reg_rating, by = 'userId') %>%
    group_by(movieYear) %>% summarize(b_m_u_my_reg = sum(rating - mu_rating - b_m_reg - b_m_u_reg)/(n() + l))
  
  Pred_EDXRatingMovieUserMovieYearReg <- EDXvalidation %>% left_join(edx_movie_reg_rating, by = 'movieId') %>%
    left_join(edx_movie_user_reg_rating, by = 'userId') %>% 
    left_join(edx_movie_user_movieYear_reg_rating, by = 'movieYear') %>%
    mutate(Pred = mu_rating + b_m_reg + b_m_u_reg + b_m_u_my_reg) %>% pull(Pred)
  
  Pred_EDXRatingMovieUserMovieYearReg
  Pred_EDXRatingMovieUserMovieYearRegCort <- if_else(Pred_EDXRatingMovieUserMovieYearReg < 0, 0, Pred_EDXRatingMovieUserMovieYearReg)
  Pred_EDXRatingMovieUserMovieYearRegCort <- if_else(Pred_EDXRatingMovieUserMovieYearRegCort > 5, 5, Pred_EDXRatingMovieUserMovieYearRegCort)
  
  rmseEDXRatingMovieUserMovieYearReg <- RMSE(EDXvalidation$rating, Pred_EDXRatingMovieUserMovieYearRegCort)
  rmseEDXRatingMovieUserMovieYearReg
})

## Plot lambda ##
EDXregtable <- data_frame(no = seq(1:length(rmses)),seq = seq(1,10,0.25), RMSE = rmses) %>% knitr::kable()
EDXregtable

qplot(seq(1,10,0.25),EDXrmses)
which.min(EDXrmses)

EDXreg <- data_frame(no = seq(1:length(rmses)),seq = seq(1,10,0.25), RMSE = EDXrmses)
EDXreg

### Minimum RMSE, Regularization Degree ###
EDXreg[which.min(rmses),]

## Regularization Application Code##
r = EDXreg[[which.min(rmses),'seq']]

edx_movie_reg_rating <- edx %>% group_by(movieId) %>% 
  summarize(b_m_reg = sum(rating - mu_rating)/(n() + r))

edx_movie_user_reg_rating <- edx %>% left_join(edx_movie_reg_rating, by = 'movieId') %>%
  group_by(userId) %>% summarize(b_m_u_reg = sum(rating - mu_rating - b_m_reg)/(n() + r))

edx_movie_user_movieYear_reg_rating <- edx %>% left_join(edx_movie_reg_rating, by = 'movieId') %>%
  left_join(edx_movie_user_reg_rating, by = 'userId') %>%
  group_by(movieYear) %>% summarize(b_m_u_my_reg = sum(rating - mu_rating - b_m_reg - b_m_u_reg)/(n() + r))

Pred_EDXRatingMovieUserMovieYearReg <- EDXvalidation %>% left_join(edx_movie_reg_rating, by = 'movieId') %>%
  left_join(edx_movie_user_reg_rating, by = 'userId') %>% 
  left_join(edx_movie_user_movieYear_reg_rating, by = 'movieYear') %>%
  mutate(Pred = mu_rating + b_m_reg + b_m_u_reg + b_m_u_my_reg) %>% pull(Pred)

rmseEDXRatingMovieUserMovieYearReg <- RMSE(EDXvalidation$rating, Pred_EDXRatingMovieUserMovieYearReg)
rmseEDXRatingMovieUserMovieYearReg

### Correction RMSE<0, RMSE>5
Pred_EDXRatingMovieUserMovieYearReg
Pred_EDXRatingMovieUserMovieYearRegCort <- if_else(Pred_EDXRatingMovieUserMovieYearReg < 0, 0,Pred_EDXRatingMovieUserMovieYearReg)
Pred_EDXRatingMovieUserMovieYearRegCort <- if_else(Pred_EDXRatingMovieUserMovieYearRegCort > 5, 5, Pred_EDXRatingMovieUserMovieYearRegCort)

rmseEDXRatingMovieUserMovieYearRegCort <- RMSE(EDXvalidation$rating, Pred_EDXRatingMovieUserMovieYearRegCort)
rmseEDXRatingMovieUserMovieYearRegCort

result <- bind_rows(result, data_frame(method = 'Validation', 
                                       RMSE = rmseEDXRatingMovieUserMovieYearRegCort))
result %>% knitr::kable()


