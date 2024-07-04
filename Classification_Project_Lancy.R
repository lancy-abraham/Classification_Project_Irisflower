#loading required packages
library(tidyverse) # visualization/processing
library(lattice)# visualization
library(ggpubr) # for multiple plots
library(GGally) # for pairplots
library(caret)  # machine learning models
library(ggplot2)
library(e1071)
library(dplyr)
library(randomForest)


#Importing the data
dataset=read.csv("C:/Users/Lancy/Desktop/iris.csv")

# View the top rows of the data
head(dataset)

# Dimensions of the data
dim(dataset)

# Column names of the data
names(dataset)

# Structure of the data
str(dataset)

# Unique values per column
lapply(dataset, function(x) length(unique(x))) 

#summary of the data
summary(dataset)

#Checking for missing values
sum(is.na(dataset))

#remove Id for easy processing data
data=dataset[,-1]
head(data)

#Found Species as character
data$Species=sapply(strsplit(as.character(data$Species),'-'), "[", 2)
str(data)

#change Species as factor
data$Species=as.factor(data$Species)
str(data)

#using boxplots to understand the distribution of attributes for each Species
p1=ggplot(data, aes(x = Species, y = SepalLengthCm,colour=Species)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(legend.position="none")

p2=ggplot(data, aes(x = Species, y = SepalWidthCm,colour=Species)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(legend.position="none")

p3=ggplot(data, aes(x = Species, y = PetalLengthCm,colour=Species)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(legend.position="none")

p4=ggplot(data, aes(x = Species, y = PetalWidthCm,colour=Species)) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.1))+
  theme(legend.position="none")

ggarrange(p1,p2,p3,p4, 
          labels = c("A", "B", "C","D"),
          ncol = 2, nrow = 2)

ggpairs(data, columns=1:4, aes(color=Species)) + 
  ggtitle("Iris Data by Species")

ggplot(data, aes(x =SepalWidthCm , y = SepalLengthCm  , color = Species))+
  geom_point()+               
  geom_smooth(method="loess", aes(fill= Species, color = Species))+
  facet_wrap(~Species, ncol = 3, nrow = 1)

ggplot(data, aes(x = PetalWidthCm , y =PetalLengthCm  , color = Species))+
  geom_point()+               
  geom_smooth(method="lm", aes(fill= Species, color = Species))+
  facet_wrap(~Species, ncol = 3, nrow = 1)

#Splitting the data for training and testing
set.seed(101)
# We use the dataset to create a partition (80% training 20% testing)
id=createDataPartition(data$Species, p=0.80, list=FALSE)

# select 80% of data to train the models
train=data[id,]
dim(train)
# select 20% of the data for testing
test=data[-id,]
dim(test)

## Histogram to understand the distribution and attributes 
hist(train$SepalWidthCm)

## Scatterplot to understand the distribution and attributes
plot(train$PetalLengthCm ~ train$PetalWidthCm, data=train)

## Box plot to understand how the distribution varies by class of flower
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(train[,i], main=names(train)[i])
}
#review the train dataset to confirm the Species are randomly selected
lapply(train, function(x) length(unique(x))) 

table(train$Species)
summary(train)
str(train)

set.seed(101)

cart_model=train(train[,1:4], train[, 5], method='rpart2')

# Predict the labels of the test set
predictions=predict(cart_model,test[,1:4])

# Evaluate the predictions
table(predictions)

# Confusion matrix 
confusionMatrix(predictions,test[,5])

#feature importance
importance_cart=varImp(cart_model)
plot(importance_cart, main="Variable Importance with cart_model")

# Train the model with preprocessing
set.seed(101)

knn_model=train(train[, 1:4], train[, 5], method='knn', 
                   preProcess=c("center", "scale"))

# Predict values
predictions=predict(knn_model,test[,1:4], type="raw")

# Confusion matrix
confusionMatrix(predictions,test[,5])

#feature importance
importance_knn=varImp(knn_model)
plot(importance_knn, main="Variable Importance with knn_model")

# Train the model with preprocessing
set.seed(101)
nnet_model=train(train[, 1:4], train[, 5], method='nnet', 
                    preProcess=c("center", "scale"), 
                    tuneLength = 2,
                    trace = FALSE,
                    maxit = 100)

# Predict values
predictions=predict(nnet_model,test[,1:4], type="raw")

# Confusion matrix
confusionMatrix(predictions,test[,5])

#feature importance
importance_nnet=varImp(nnet_model);importance_nnet

# Train the model with preprocessing
set.seed(101)
randomforest_model=train(train[, 1:4], train[, 5], method='rf')

# Predict values
predictions=predict(randomforest_model,test[,1:4], type="raw")

# Confusion matrix
confusionMatrix(predictions,test[,5])

#feature importance
importance_rf=varImp(randomforest_model)
plot(importance_rf, main="Variable Importance with randomforest_model")

models_compare=resamples(list(cart_model,knn_model, nnet_model,randomforest_model))

# Summary of the models performances
summary(models_compare)

# Dotplot of the models performances
dotplot(models_compare)



