# Project: `MovieLens`
# Author: Ruben R. Kazumov <kazumov@gmail.com>
# URL: "https://github.com/kazumov/movielens"
# Course: HarvardX PH125.9x
#

library(tidyverse)
library(lubridate)
library(crayon)

# Data transforming ----

# * Arguments ----

# The following code expects availability of the data objects:
# i) `edx.Rda` and 
# ii) `validation.Rda`
#

# load("edx.Rda")
# load("validation.Rda")

# * Variables, constants ----

x = edx
y = validation

# * Joining the data ----

x <- x %>% mutate(train = TRUE) # during the fitting the data will be separated by vector `train`
y <- y %>% mutate(train = FALSE)
x <- rbind(x, y)
rm(y)

# * Indexing and adjusting the vector types ----

x <- x %>% 
  mutate(
    id = as.character(seq(from = 1, to =dim(x)[1], by = 1)),
    userId = as.character(userId),
    movieId = as.character(movieId),
    dateTime = lubridate::as_datetime(timestamp),
    ratingNumeric = rating,
    rating = factor(rating, levels = seq(from = 0.5, to = 5, by = 0.5))) %>%
  select(-timestamp)

# * Extracting the date of the halfs-ratings introduction ----

halfsIntroduced <- x %>% 
  filter(rating %in% factor(c(0.5, 1.5, 2.5, 3.5, 4.5))) %>%
  .$dateTime %>% 
  min()

# * The data of two periods subsetting ----

x <- x %>% mutate(halfRatingPossible = dateTime > halfsIntroduced)

rm(halfsIntroduced)

# * Extracting the list of unique movies ----

titleYearMatchRegexpr <- "^(.+) (\\()(\\d{4})(\\))$" 
movies <- x %>% distinct(movieId, title, genres) %>%
  mutate(movieYear = as.numeric(stringr::str_match(title, titleYearMatchRegexpr)[, 4]), 
         movieTitle = stringr::str_match(title, titleYearMatchRegexpr)[, 2]) %>%
  select(-title)

rm(titleYearMatchRegexpr)

# * Extracting information about list of genres in each move ----

movieGenresStr <- movies %>%
  select(movieId, genres)

movieGenresList <- apply(X = movieGenresStr, MARGIN = 1, FUN = function(df){
  genres <- unname(unlist(strsplit(df["genres"], split = "\\|")))
  movieIds <- rep(x = df["movieId"], times = length(genres))
  return(data.frame(movieId = movieIds, genre = genres, stringsAsFactors = FALSE))
})

movieGenres <- bind_rows(movieGenresList)

rm(movieGenresStr, movieGenresList)

# * Definig the movie by the genres binary mask ----

movies <- merge(x = movies, y = movieGenres, by.x = "movieId") %>%
  mutate(check = TRUE) %>%
  mutate(check = as.numeric(1)) %>%
  select(-genres) %>%
  spread(key = genre, value = check, fill = 0)

# * Extracting the summary statistics for each movie ----

movieStatistics <- x %>% 
  group_by(movieId) %>% 
  summarise(movieViewed = n(), 
            movieAvgRating = mean(ratingNumeric))

movies <- merge(x = movies, y = movieStatistics, by.x = "movieId")

rm(movieStatistics)

# * Studying user genre preferences and rating activity (IT TAKES TIME) ----

userProfile <- x %>% 
  select(userId, movieId, ratingNumeric)

moviesMasks <- movies %>% 
  select(-movieYear, -movieTitle, -movieViewed, -movieAvgRating)

userProfile <- merge(x = userProfile, y = moviesMasks, by = "movieId") 

userProfile <- userProfile %>% 
  tidyr::gather(key = "genre", value = "check", 4:23)

userProfile <- userProfile %>% 
  mutate(rated = ifelse(check, ratingNumeric, 0))

userProfile <- userProfile %>% 
  group_by(userId, genre) %>%
  summarise(cumulative = sum(rated)) %>%
  ungroup() %>%
  mutate(genre = paste("userOn", genre, sep = "")) %>% 
  tidyr::spread(key = genre, value = cumulative)

userStatistics <- x %>%
  group_by(userId) %>%
  summarise(userAvgRating = mean(ratingNumeric),
            userTotalRatings = n())

users <- merge(x = userProfile, y = userStatistics, by.x = "userId")

users[, 2:21] <- users[, 2:21] / users[, 23] # averaging the genre cumulative raiting

users <- users %>% select(-userTotalRatings)

rm(userProfile, userStatistics, moviesMasks)

# * Composing the final data representation ----

ratings <- x %>% select(id, userId, movieId, rating, ratingNumeric, halfRatingPossible, train)
ratings <- merge(x = ratings, y = users, by.x = "userId")
ratings <- merge(x = ratings, y = movies, by.x = "movieId")

# * Composing the user sympaty coefficient ----

movieGenreMask <- ratings %>% select(Comedy, Romance, Action, Crime, Thriller, Drama, `Sci-Fi`, Adventure, Children, Fantasy, War, Animation, Musical, Western, Mystery, 
                                     `Film-Noir`, Horror, Documentary, IMAX, `(no genres listed)`)
userGenreNormRating <- ratings %>% select(Comedy, userOnRomance, userOnAction, userOnCrime, userOnThriller, userOnDrama, `userOnSci-Fi`, userOnAdventure, userOnChildren, 
                                          userOnFantasy, userOnWar, userOnAnimation, userOnMusical, userOnWestern, userOnMystery, `userOnFilm-Noir`, userOnHorror, userOnDocumentary, 
                                          userOnIMAX, `userOn(no genres listed)`)
id <- ratings %>% select(id)

sympathy <- data.frame(id = id, sympathy = rowSums((movieGenreMask * userGenreNormRating)))

ratings <- ratings %>% 
  select(id, userId, movieId, rating, ratingNumeric, halfRatingPossible, train, movieAvgRating, userAvgRating)

ratings <- merge(x = ratings, y = sympathy, by.x = "id")

rm(movieGenreMask, userGenreNormRating, id, sympathy)

# * The final data model ----

db <- ratings %>% select(rating, 
                         ratingNumeric, 
                         sympathy, 
                         movieAvgRating, 
                         userAvgRating, 
                         halfRatingPossible, 
                         train)

rm(ratings, x)

saveRDS(db, file = "db.Rds") # store in working directory

# Building predictive model ----

# * Utilites ----

RMSE <- function(y, yBar){
# Calculates RMSE for the numerical vectors
# Arguments:
#   y - Numeric vector of testing data
#   yBar - Numeric vector of predicted data
#
  return(
    sqrt(
      mean(
        (y - yBar)^2
      )  
    )
  )
} 

RMSEFacors <- function(yFactor, yBarFactor){
# Calculates RMSE for the vector of factors
# Arguments:
#   yFactor - Factor vector of testing data
#   yBarFactor - Factor vector of predicted data
# Notes:
#  Vectors yFactor and yBarFactor should have the same levels.
#
  y <- as.numeric(as.character(yFactor))
  yBar <- as.numeric(as.character(yBarFactor))
  return(
    RMSE(y = y, yBar = yBar)
  )
} 

report <- data.frame(type = as.character(),
                     library = as.character(),
                     sampleSize = as.integer(),
                     rmse = as.numeric(),
                     notes = as.character())

# * Classification ----

library(caret)

wholeRatingsLevels <- seq(from = 1, to = 5, by = 1)
halfsRatingsLevels <- seq(from = 0.5, to = 5, by = 0.5)

trainWholes <- db %>% 
  filter(train == TRUE, halfRatingPossible == FALSE) %>%
  mutate(rating = factor(rating, levels = wholeRatingsLevels)) %>%
  select(-train, -halfRatingPossible, -ratingNumeric)

trainHalfs  <- db %>% 
  filter(train == TRUE, halfRatingPossible == TRUE) %>%
  select(-train, -halfRatingPossible, -ratingNumeric)

testWholes <- db %>% 
  filter(train == FALSE, halfRatingPossible == FALSE) %>%
  mutate(rating = factor(rating, levels = wholeRatingsLevels)) %>%
  select(-train, -halfRatingPossible, -ratingNumeric)

testHalfs <- db %>% 
  filter(train == FALSE, halfRatingPossible == TRUE) %>%
  select(-train, -halfRatingPossible, -ratingNumeric)

# * * Classification with randomForest algorithm ----

library(randomForest)

set.seed(31415)

randomForestTrainSampleSize = 5e5 # sample size 0.5M

# * * * Wholes ----

trainSample <- trainWholes %>%
  sample_n(randomForestTrainSampleSize)

x = trainSample %>% 
  select(-rating)

y = trainSample$rating

set.seed(31415)

# the fitting calculation process takes more than 10 minutes
# the result object fitRFWholes has size 533.9MB
fitRFWholes <- randomForest(x = x, y = y, ntree = 200)

predictY <- predict(object = fitRFWholes, 
                        newdata = testWholes %>% select(-rating))

rmse <- RMSEFacors(yFactor = testWholes$rating, yBarFactor = predictY)

r <- tribble(
  ~type,  ~library, ~sampleSize, ~rmse, ~notes,
  "classification", "randomForest", 500000, rmse, "5 ratings set; nTree = 200"
)

report <- rbind(report, r)

# * * * Halfs ----

trainSample <- trainHalfs %>%
  sample_n(randomForestTrainSampleSize)

x = trainSample %>% 
  select(-rating)

y = trainSample$rating


set.seed(31415)

# the fitting calculation process takes more than 10 minutes
# the result object fitRFHalfs has size 665MB
fitRFHalfs <- randomForest(x = x, y = y, ntree = 200)

predictY <- predict(object = fitRFHalfs, 
                    newdata = testHalfs %>% select(-rating))

rmse <- RMSEFacors(yFactor = testHalfs$rating, yBarFactor = predictY)

r <- tribble(
  ~type,  ~library, ~sampleSize, ~rmse, ~notes,
  "classification", "randomForest", 500000, rmse, "10 ratings set; nTree = 200"
)

report <- rbind(report, r)

# * * Classification with Rborist algorithm ----

library(Rborist)

set.seed(31415)

rboristTrainSampleSize = 1e6 # sample size 1M

# * * * Wholes ----

trainSample <- trainWholes %>%
  sample_n(rboristTrainSampleSize)

x = trainSample %>% 
  select(-rating)

y = trainSample$rating

set.seed(31415)

# the fitting calculation process takes about 10 minutes
# the result object fitRboristWholes has size 1.6GB
fitRboristWholes <- Rborist(x = x, y = y)

predictY <- predict(object = fitRboristWholes, 
                    newdata = testWholes %>% select(-rating))

rmse <- RMSEFacors(yFactor = testWholes$rating, yBarFactor = predictY$yPred)

r <- tribble(
  ~type,  ~library, ~sampleSize, ~rmse, ~notes,
  "classification", "Rborist", 1000000, rmse, "5 ratings set"
)

report <- rbind(report, r)

# * * * Halfs ----

trainSample <- trainHalfs %>%
  sample_n(rboristTrainSampleSize)

x = trainSample %>% 
  select(-rating)

y = trainSample$rating

set.seed(31415)

# the fitting calculation process takes about 10 minutes
# the result object fitRboristHalfs has size 3.5GB
fitRboristHalfs <- Rborist(x = x, y = y)

predictY <- predict(object = fitRboristHalfs, 
                    newdata = testHalfs %>% select(-rating))

rmse <- RMSEFacors(yFactor = testHalfs$rating, yBarFactor = predictY$yPred)

r <- tribble(
  ~type,  ~library, ~sampleSize, ~rmse, ~notes,
  "classification", "Rborist", 1000000, rmse, "10 ratings set"
)

report <- rbind(report, r)

saveRDS(report, file = "report.Rds")

# * Regression ----

train <- db %>% 
  filter(train == TRUE) %>%
  select(-train, -halfRatingPossible, -rating)

test <- db %>% 
  filter(train == FALSE) %>%
  select(-train, -halfRatingPossible, -rating)

set.seed(31415)

# the result object fitLm has size 510.2MB
fitLm <- lm(formula = ratingNumeric ~ ., data = train)

predictionY <- predict(object = fitLm, newdata = test)

rmse <- RMSE(test$ratingNumeric, predictionY)

r <- tribble(
  ~type,  ~library, ~sampleSize, ~rmse, ~notes,
  "regression", "lm", dim(train)[1], rmse, "full population"
)

report <- rbind(report, r)

saveRDS(report, file = "report.Rds")

# Report ----

report







