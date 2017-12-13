# Clear workspace
rm(list=ls())

# Load relevant packages
library(dplyr)
library(caret)

# Read rating data
rated <- readr::read_delim(
  file = file.path("Memory","rated_movies.txt"),
  delim = "\t"
)

# Load the corpus matrix dtm
load(file.path("Memory","dtm.r"))

# Subset rated to match titles in dtm
rated <- rated[rated$title %in% rownames(dtm), , drop = FALSE]

# Convert to tibble and add the ratings data
dtm <- dtm %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Rating = rated$rating
  )

# Segregate the data into training and testing sets
set.seed(8)
train_indices <- caret::createDataPartition(
  1:nrow(dtm), times = 1, p = 0.75, list = FALSE
)

dtm_train <- dtm[train_indices,]
dtm_test <- dtm[-train_indices,]

# Train a regression model
tr_control <- trainControl(method = "repeatedcv", repeats = 1, p = 0.8)
hist(dtm_train$Rating)
outlier_free_train <- dtm_train %>%
  dplyr::filter(Rating > 0, Rating <= 10)
pca_train <- FactoMineR::PCA(outlier_free_train %>% dplyr::select(-Rating), ncp = 5)
pca_train_coords <- pca_train$ind$coord %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(Rating = outlier_free_train$Rating)
pca_test <- FactoMineR::predict.PCA(
  pca_train,
  dtm_test %>% dplyr::select(-Rating)
)
pca_test_coords <- pca_test$coord %>% 
  tibble::as_tibble() %>%
  dplyr::mutate(Rating = dtm_test$Rating)
model <- train(
  Rating ~ ., data = pca_train_coords,
  method = "rf",
  trControl = tr_control
)

predict(model, newdata = pca_test_coords) - pca_test_coords$Rating

plot(pca_test_coords$Rating, predict(model, newdata = pca_test_coords) - pca_test_coords$Rating)
plot(pca_train_coords$Rating, predict(model, newdata = pca_train_coords) - pca_train_coords$Rating)

plot(pca_train_coords$Rating, predict(model, newdata = pca_train_coords))

cor.test(
  pca_train_coords$Rating,
  predict(model, newdata = pca_train_coords)
)
