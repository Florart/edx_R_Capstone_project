if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip
filename <- "comeon.R"

  dl <- tempfile()
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
  
  ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                   col.names = c("userId", "movieId", "rating", "timestamp"))
  
  movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
  colnames(movies) <- c("movieId", "title", "genres")
  

  # if using R 4.0 or later:
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
  
  #glimpse(edx)
  #we want to check the exact year movie was rated
  #so we convert timestamp to year
  edx <- mutate(edx,year_rated= year(as_datetime(timestamp)))
  head(edx)
  
 #data cleaning
  #we want to see the number of unique movies we have without repetition
  n_distinct(edx$movieId)
  #we also need to see number of unique users using userId who rated our movies
  n_distinct(edx$userId)
  #we start exploring our data set
  #firstly let's take a look on our data structure using str function
  str(edx)
  #now let's see our movies summary which presents statistical summary of our data set
  summary(edx)
  
  #we need to see the most watched movies from our data set
  edx %>%
    group_by(title) %>%
    summarize(count = n()) %>%
    arrange(-count) %>%
    top_n(10, count) %>%
    ggplot(aes(count, reorder(title, count))) +
    geom_bar(color = "black", fill = "deepskyblue2", stat = "identity") +
    xlab("Count") +
    ylab(NULL) +
    theme_bw()
  #the highest rate given to movies by users
  edx %>%
    ggplot(aes(rating, y = ..prop..)) +
    geom_bar(color = "black", fill = "deepskyblue2") +
    labs(x = "Ratings", y = "Relative Frequency") +
    scale_x_continuous(breaks = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) +
    theme_bw()
  #Analysis and results
  #we are going to use different model adn the residual mean square error(RMSE) to measure the accuracy and quantify typical error we make when predicting the movie rating
  #the rmse formula RMSE = \sqrt{\frac{1}{N}\displaystyle\sum_{u,i} (\hat{y}_{u,i}-y_{u,i})^{2}} $$
  #where; N is the number of users, movie ratings, and the sum incorporating the total combinations.
  
  ## our first model is Simple Prediction Based on Mean Rating
  aver <- mean(edx$rating)
  aver
  simplermse<-RMSE(validation$rating,aver)
  simplermse
  ## save the result from rmse using simple mean
  simple_rmse = data_frame(method= "Simple Mean model",
                           RMSE=simplermse)
  simple_rmse %>% knitr::kable()
  #second method to be used to analyse our data is Movie Effect model by simple model taking consideration of movie bias b_i
  aver <- mean(edx$rating)
  aver
  movies_aver <- edx %>% group_by(movieId) %>% summarise(b_i= mean(rating-aver))
  p_rating <- aver + validation %>%
    left_join(movies_aver, by="movieId") %>%
    pull(b_i)
  rmse_modelmoviebias <- RMSE(p_rating, validation$rating)
  rmse_modelmoviebias
  simple_rmse <- bind_rows(simple_rmse, tibble(Method = "movie bias model", 
                                               RMSE=rmse_modelmoviebias))
  simple_rmse %>% knitr::kable()
  
  #the third model used to reduce typical error is movies and users effect model
  #firstly we need user average taking into consideration movies
user_aver <- edx %>%
    left_join(movies_aver, by="movieId") %>%
    group_by(userId)%>%
    summarise(b_u = mean(rating - aver - b_i))
p_rating <- validation %>%
  left_join(movies_aver, by="movieId")%>%
  left_join(user_aver, by="userId") %>%
  mutate(pred = aver + b_i + b_u)%>%
  pull(pred)

rmse_user_effect <- RMSE(p_rating,validation$rating)
rmse_user_effect
simple_rmse <- bind_rows(simple_rmse,
                         data_frame(method="Movie-User effects",
                                    RMSE=rmse_user_effect))
simple_rmse %>% knitr::kable()

#the 4th method used is regularization
#we use lambdas
lambdas <- seq(0,5,0.5)
reguralizationrmses <- sapply(lambdas, function(x){
  aver <- mean(edx$rating)
  
  b_i <- edx %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - aver)/(n()+x))
  b_u <- edx %>%
    left_join(b_i, by="movieId")%>%
    group_by(userId)%>%
    summarise(b_u = sum(rating - b_i - aver)/(n()+x))
  p_rating <- validation %>%
    left_join(b_i, by ="movieId")%>%
    left_join(b_u, by ="userId") %>%
    mutate(pred = aver + b_i + b_u)%>%
    pull(pred)
  
  return(RMSE(p_rating, validation$rating))
})
rmes_regul <- min(reguralizationrmses)
rmes_regul

#we plot RMSE against lambdas

qplot(lambdas,reguralizationrmses)
lambda<- lambdas[which.min(reguralizationrmses)]
lambda

simple_rmse<-bind_rows(simple_rmse,
                       data_frame(method="Regularization model",
                                  RMSE= rmes_regul))
simple_rmse %>% knitr::kable()