library(DataExplorer)
library(class) #Required library to use the knn() function
library(gmodels) #Required library to evaluate prediction error
library(caret) #Required package to evaluate prediction error 

prc <- Prostate_Cancer
#prc <- na.omit(prc1)

# Viewing the variables in our dataset

plot_str(prc) # Check size of plot

# Removing the first column (id)
prc <- prc[-1]

# Distribution of our target variable
table(prc$diagnosis_result)

# Normalizing function for numeric variables
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x,na.rm = TRUE) - min(x, na.rm = TRUE))) }

# Our first column in the dataset is diagnostic_result which is not numeric
# Hence we go ahead and normalize the remaining features

prc_norm <- as.data.frame(lapply(prc[2:9], normalize))
head(prc_norm)

# Confirming whether the features are normalized
summary(prc_norm$fractal_dimension)
summary(prc_norm$radius)

# Splitting our dataset into a training and test dataset with a 65-35 ratio
prc_train <- prc_norm[1:65,]

prc_test <- prc_norm[66:100,]

#Our target variable is 'diagnosis_result' which we have not included in our training and test data sets.

prc_train_labels <- prc[1:65,1, drop = TRUE]
prc_test_labels <- prc[66:100,1, drop = TRUE]  


# Ideal value for k is usually the square root of the total number of observations which in this case is 10
knn.10 <- knn(train = prc_train, test = prc_test,cl = prc_train_labels, k=10)

# We can look at our results in a confusion matrix
confusionMatrix(table(knn.10, prc_test_labels), positive = 'M')

# Computing accuracy for the model with k = 10
Accuracy <- 100 * sum(prc_test_labels == knn.10)/NROW(prc_test_labels)


# Computing the accuracy for the model with different values of k 
i=1
k.optm=1
for (i in 1:20){
   knn.mod <- knn(train=prc_train, test=prc_test, cl=prc_train_labels, k=i)
   k.optm[i] <- 100 * sum(prc_test_labels == knn.mod)/NROW(prc_test_labels)
   k=i
   cat(k,'=',k.optm[i],'
        ')
}

# Since k = 6 had the best accuracy, we create a knn model with 6 neighbors and look at our results in a confusion matrix

knn.6 <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k = 6)
confusionMatrix(table(knn.6,prc_test_labels), positive = 'M' )
