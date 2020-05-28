library(dplyr)
library(DataExplorer)
library(rpart)
library(rpart.plot)

set.seed(678)
path <- 'https://raw.githubusercontent.com/thomaspernet/data_csv_r/master/data/titanic_csv.csv'
titanic <- read.csv(path)

head(titanic)
tail(titanic)


# The head and tail commands help us realize that the data is not shuffled. 
# We need to use the sample() command to shuffle the dataset so that the training and test set
# cover features of all different passengers

shuffle_index <- sample(1:nrow(titanic))



titanic <- titanic[shuffle_index, ]
head(titanic)

# In order to clean our dataset we-
# Create factor variables for pclass and survived
# Drop all the NA values
# Remove the home.dest, cabin, name, X and ticket features from our data

clean_titanic <- titanic %>%
  select(-c(home.dest, cabin, name, X, ticket)) %>% 
  #Convert to pclass and survived to factor level
  mutate(pclass = factor(pclass, levels = c(1, 2, 3), labels = c('Upper', 'Middle', 'Lower')),
         survived = factor(survived, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  #Removing all NA values
  na.omit()
glimpse(clean_titanic)

# We now need to create a training and test set where we split the data 80-20 

# We create a function with three arguments which creates the training set
# The size specifies the proportion of split and the argument train returns the train set when TRUE and test set when FALSE

create_train_test <- function(clean_titanic, size = 0.8, train = TRUE) {
  n_row = nrow(clean_titanic)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (clean_titanic[train_sample,])
  } else 
    return (clean_titanic[-train_sample,])
  
}

data_train <- create_train_test(clean_titanic, 0.8, train = TRUE)
data_test <- create_train_test(clean_titanic, 0.8, train = FALSE)


dim(data_train)
dim(data_test)

# We see the distribution of our outcome variable in the test and training set hoping for an even distribution
prop.table(table(data_train$survived))
prop.table(table(data_test$survived))

# We now fit our model where survived~. indicates the formula of the decision tree, data represents the dataset
# and the method argument is denoted by 'class' for a classification tree and 'anova' for a regression treew
fit <- rpart(survived~., data = data_train, method = 'class')
rpart.plot(fit, extra = 106)

predict_unseen <-predict(fit, data_test, type = 'class')
table_mat <- table(data_test$survived, predict_unseen)
table_mat

accuracy <- sum(diag(table_mat)) / sum(table_mat)
#print(paste('Accuracy:', accuracy_Test))


# Function for calculating accuracy for different model fits
accuracy_tune <- function(fit) {
  predict_unseen <- predict(fit, data_test, type = 'class')
  table_mat <- table(data_test$survived, predict_unseen)
  accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
  accuracy_Test
}


# Tuning the hyper-parameters for our model

control <- rpart.control(minsplit = 4,
                         minbucket = round(5 / 3),
                         maxdepth = 3,
                         cp = 0)
tune_fit <- rpart(survived~., data = data_train, method = 'class', control = control)
accuracy_tune(tune_fit)
