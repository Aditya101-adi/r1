#Aditya Pal Chaudhuri
#Task 6 Prediction using Decision Tree Algorithm
library(rpart)
library(caret)

data=read.csv("C:/Users/DP/Desktop/iris.csv")         #Reading the given data
head(data)
data=data[-1]                                         #Removing id
data$Species= as.factor(data$Species)                 #Converting Species to Categorical data

tree1 <- rpart(Species ~ . , data, method = 'class')  #Fitting a decision tree model

summary(tree1)                                        #Details of the decision tree model
#Petal Width and length are the most important features in classifying species respectively followed by Sepal Length
#and width respectively which are not as important

rpart.plot(tree1, type=2)                             #Plotting the decision tree

ggplot(data= data, aes(x= PetalLengthCm,y= PetalWidthCm ,col= Species))+ geom_point()+ theme_bw()+
                      geom_vline(xintercept=2.5, linetype="dashed", color = "red")+
                      geom_hline(yintercept=1.8, linetype="dashed", color = "blue")
#Representing the tree model showing the splits on Sepal length vs Width

predicted_val1 <- predict(tree1, data, type='class')
confusionMatrix(predicted_val1,as.factor(data$Species))
#It shows how accurately the tree predicts Species of the data it is trained on

#It correctly classifies 144 out of 150 species from the data it is trained. The depth of the tree is 2 and the number
#of features is 4, thus there doesnot seems to be any overfitting. The model is highly accurate 0.96 and seems to work
#very nicely on the data it is trained on.

#To test if we feed any new data to this classifier, would it be able to predict the right class accordingly, we use
#random spliting of data to check how the model performes on test data to have an idea how good it is for new data. 
data_sample = sample.split(data$Species,SplitRatio=0.80)

#Training Data consist of 0.8 parts of the entire data
train_data = subset(data,data_sample==TRUE)

test_data = subset(data,data_sample==FALSE)
#We test our model's classification on 30 obervations in the test data
tree2 <- rpart(Species ~ . , train_data, method = 'class')

rpart.plot(tree2, type=1)

predicted_val2 <- predict(tree2, test_data, type='class')
confusionMatrix(predicted_val2,as.factor(test_data$Species))
# out of 30 classifications are correct. The classifications are done on the data decision tree is not trained on. 
#This shows decision tree is a trustworthy and robust model for the iris data and the high accuracy shows 
#classifications done by the model on new data is expected to be of the correct class and reliable.

#tree1 is our final decision tree model