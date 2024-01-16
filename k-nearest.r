############  GET THE DATA USING ISLR LIBRARY ##############
library(ISLR)
print(str(Caravan))
print(summary(Caravan$Purchase))
##################### CLEAN THE DATA ##########################
##### CHECK FOR NULL VALUES #############
print(any(is.na(Caravan)))
############ STANDARDIZE THE VARIABLE ##################
####### CHECK THE VARIANCE FOR FIRST COLUMN ########
print(var(Caravan[,1]))
####### CHECK THE VARIANCE FOR SECOND COLUMN ########
print(var(Caravan[,2]))
################## save the Purchase column in a separate variable  ######################
purchase <- Caravan[,86]
##################### Standarize the dataset using "scale()" R function  ################
standardized.Caravan <- scale(Caravan[,-86])
####### CHECK THE VARIANCE FOR FIRST COLUMN ########
print(var(standardized.Caravan[,1]))
####### CHECK THE VARIANCE FOR SECOND COLUMN ########
print(var(standardized.Caravan[,2]))
############## First 100 rows for test set  #################
test.index <- 1:1000
test.data <- standardized.Caravan[test.index,]
test.purchase <- purchase[test.index]
##############  Rest of data for training  ##################
train.data <- standardized.Caravan[-test.index,]
train.purchase <- purchase[-test.index]
#####################
####################   KNN MODEL ##############################
#####################
library(class)
set.seed(101)
predicted.purchase <- knn(train.data,test.data,train.purchase,k=1)
print(head(predicted.purchase))
##############  evaluate the model we trained and see our misclassification error rate.  ##############
misclass.error <- mean(test.purchase != predicted.purchase)
print(misclass.error)
############# CHOOSING K VALUE ##########################
predicted.purchase <- NULL
error.rate <- NULL

for(i in 1:20){
    set.seed(101)
    predicted.purchase = knn(train.data,test.data,train.purchase,k=i)
    error.rate[i] = mean(test.purchase != predicted.purchase)
}
print(error.rate)
###################  VISUALIZE K ELBOW METHOD ##################
library(ggplot2)
k.values <- 1:20
error.df <- data.frame(error.rate,k.values)
print(error.df)
########### create a scatter plot ###########
pl <- ggplot(error.df,aes(x=k.values,y=error.rate)) + geom_point()+ geom_line(lty="dotted",color='red')
print(pl)