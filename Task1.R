library(MASS)
data=read.csv("C:/Users/DP/Desktop/scores.csv")
data
a=data$Hours
b=data$Scores
model=lm(b~a)
summary(model)
predict(model,data.frame(a=9.25))
library(ggplot2)
ggplot(data,aes(x = a, y = b)) + geom_point() +geom_smooth(method = "lm")+ labs(title = "Student Percentage vs Study Hours",
                                                                                x = "Study Hours",
                                                                                y = "Student Percentage")
fits=predict(model,data.frame(a=data$Hours))
resi=stdres(model)
ggplot(data,aes(x = fits, y = resi)) + geom_point() + geom_hline(yintercept=0)+  theme_bw()+
                                                          geom_hline(yintercept=-2, linetype="dashed", color = "red")+
                                                          geom_hline(yintercept=2, linetype="dashed", color = "red")+
                                                          labs(title = "Standardized Residuals vs Fitted Values",
                                                                                x = "Fitted Values",
                                                                                y = "Standardized Residuals")
                                                          
                                                          