library(MASS)
library(ggplot2)

data=read.csv("C:/Users/DP/Desktop/scores.csv") #Reading the given data
data

gdata=data.frame(h=9.25)                        #Storing the given study hour

h=data$Hours                                    #Extracting the hours of study
s=data$Scores                                   #Extracting the scores

model=lm(s~h)                                   #Fitting a linear regression model in 2 variables of the form
                                                #s=	β0+ β1*h+ ε

summary(model)                                  #Details of the regression fit
#We obtain a regression model of the form:      s= 2.4837+  9.7758*h
#Measure of goodness of fit: R-Sq = 95.29% shows this model is a good fit

predict.lm(model,data.frame(h=9.25))     #Predicting the score value for the given study hour

ggplot(data,aes(x = h, y = s)) + geom_point() +geom_smooth(method = "lm")+ 
                      labs(title = "Student Percentage vs Study Hours",x = "Study Hours",y = "Student Percentage")
                      #Representing the regression fit of Students' Scores on Study Hours
#The predicted value is 92.91 (rounding up)

fits=predict(model,data.frame(a=data$Hours))    #Storing the regression fits
resi=stdres(model)                              #Storing the standardized residuals

ggplot(data,aes(x = fits, y = resi)) + geom_point() + geom_hline(yintercept=0)+  theme_bw()+
                                                    geom_hline(yintercept=-2, linetype="dashed", color = "red")+
                                                    geom_hline(yintercept=2, linetype="dashed", color = "red")+
                                                    labs(title = "Standardized Residuals vs Fitted Values",
                                                    x = "Fitted Values",  y = "Standardized Residuals")
                       #Representing the Residual values vs Fitted Values of the linear model through a Scatterplot

#Regression Analysis: As the standardized residuals are randomly scattered, the model seems to be trustworthy 
#explaining most of the deterministic component. There seems to be only one potential influencial point with 
#standardized residual value less than -2 showing it's pretty robust. Otherwise, the model is a good one and the 
#predicted score of 92.91 can we considered quite close to the actual score for 9.25 hours of study.  
                                                          
                                                          
