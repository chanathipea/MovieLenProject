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

str(edx)
dim(edx)

colnames(edx)
edx %>% group_by(rating) %>% summarize(number = n())
uniqueN(edx$movieId)
uniqueN(edx$userId)

edx %>% filter(genres%in%c("Drama","Comedy","Thriller","Romance")) %>% 
                 group_by(genres) %>% summarize(number = n())
edx %>% mutate(Comedy = str_detect(genres, "Comedy"),
               Romance = str_detect(genres, "Romance"),
               Thriller = str_detect(genres, "Thriller"),
               Drama = str_detect(genres, "Drama")) %>% 
        summarize(Comedy = sum(Comedy),
                  Romance = sum(Romance),
                  Thriller = sum(Thriller),
                  Drama = sum(Drama))
                                                                  
edx %>% group_by(title)%>%summarize(n = n()) %>% arrange(desc(n))
edx %>% group_by(rating)%>%summarize(n = n()) %>% arrange(desc(n))

edx %>% mutate(decimal = ifelse((rating%%1)==0,TRUE,FALSE)) %>% summarize(mean(decimal))
