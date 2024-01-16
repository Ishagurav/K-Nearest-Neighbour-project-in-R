library(ISLR)
print(head(iris))
print(str(iris))
################ STANDARDIZE THE FEATURES #################
stand.features <- scale(iris[1:4])
############# CHECK THE VARIANCE OF FIRST COLUMN ############
print(var(stand.features[,1]))
##############  Join the standardized data with the response/target/label column (the column with the species names.
final.data <- cbind(stand.features,iris[5])
print(head(final.data))
############### TRAIN TEST SPLIT ###################
set.seed(101)

library(caTools)

sample <- sample.split(final.data$Species, SplitRatio = .70)
train <- subset(final.data, sample == TRUE)
test <- subset(final.data, sample == FALSE)
################### build a knn model #################
library(class)
predicted.species <- knn(train[1:4],test[1:4],train$Species,k=1)
print(predicted.species)
######## calculate missclassification rate ##############
mis.err <- mean(test$Species != predicted.species)
print(mis.err)
############### choose a k value ###############
predicted.species <- NULL
error.rate <- NULL

for(i in 1:10){
    set.seed(101)
    predicted.species <- knn(train[1:4],test[1:4],train$Species,k=i)
    error.rate[i] <- mean(test$Species != predicted.species)
}
############# create a scatter plot #################
library(ggplot2)
k.values <- 1:10
error.df <- data.frame(error.rate,k.values)
print(error.df)
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
print(pl)