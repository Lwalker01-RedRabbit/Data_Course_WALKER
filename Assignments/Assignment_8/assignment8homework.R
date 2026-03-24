#assignment goals: 
#find the best method for the data set of growing mushrooms. 
#library stuff to load and look for best model/method for data set. 
library(modelr)
#library(easystats)(Don't want to fix this)
library(broom)
library(tidyverse)
#library(fitdistrplus)(IDK what this one is)

library(ggplot2)

#what dependent variable do we want to know about musshies? 
#what are our explainatory variables? 


#load data set
mushies <- read.csv("mushroom_growth.csv")

#look at data set
head(mushies)
str(mushies)
# we have 6 sets of data to work with. 6 $ thingies. 



#model 1: 
m1 <- lm(GrowthRate ~ Temperature, data = mushies)
summary(m1)

#make plot??
ggplot(mushies, aes(x= Temperature, y = GrowthRate))+
  geom_point() +
  geom_smooth(method = "lm")
#yeah I can't see anything with this. That line is nice tho

predict(m1)
#predict if temp is 25.
predict(m1, newdata = data.frame(Temperature = 25))
#1. 104.0281
mushies$pred_m1 <- predict(m1)

#let's plot actual vs predicted 
ggplot(mushies, aes(x=Temperature,y=GrowthRate))+
  geom_point()+
  geom_line(aes(y=pred_m1), color = "steelblue")
#nice little line tells us the prediction, with hotter temp 25 decreasing the growth rate
#let's add more variables 
#method 2 
m2 <- lm(GrowthRate ~ Temperature + Humidity, data = mushies)
#plug everything into what I did for m1 
summary(m2)
predict(m2)
mushies$pred_m2 <- predict(m2)

#Bell curve?
#this would be the bell curve of predicted values
ggplot(data.frame(pred_m2), aes(x=pred_m2))+
  geom_histogram(aes(y = ..density..), bins= 15)+
  stat_function(fun = dnorm,
                args = list(mean = mean(pred_m2),
                            sd= sd(pred_m2)))

#bell curve of actual values. 
res_m2 <- residuals(m2)

ggplot(data.frame(res_m2), aes(x = res_m2)) +
  geom_histogram(aes(y = after_stat(density)), bins = 15) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(res_m2),
      sd = sd(res_m2)))

#model 3
m3 <- lm(GrowthRate ~ Temperature + Humidity + Nitrogen, data = mushies)
#predictions 
summary(m3)
predict(m3)
mushies$pred_m3 <- predict(m3)

#plot explaining nitrogen and growth rate
ggplot(mushies, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)






#model 4
m4 <- lm(GrowthRate ~ Temperature + Humidity + Nitrogen + Light, data = mushies)
#predictions 
summary(m4)
predict(m4)
mushies$pred_m4 <- predict(m4)

#plot explaining light and growth rate 
ggplot(mushies, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

#to help find the best method from m1, m2, m3, and m4
#we want to use the mean squared error from each residual. 
#so, MSE = mean(residuals^2)
#MSE = mean(residuals^2)

#let's put some code together to calculate MSE for each model.
mse_m1 <- mean(residuals(m1)^2)
mse_m2 <- mean(residuals(m2)^2)
mse_m3 <- mean(residuals(m3)^2)
mse_m4 <- mean(residuals(m4)^2)

mse_m1
mse_m2
mse_m3
mse_m4
#we know that the lower mean residual, means that the better model. 
#looks like our light model has the lowest value at 5731.211
#model 4 is the best model. 
#Here is some code that will select the best model. 
MSEvals <- data.frame(Model = c("m1","m2","m3","m4"),
                      MSE = c(mse_m1,mse_m2,mse_m3,mse_m4))

BEST <- MSEvals[which.min(MSEvals$MSE),]
BEST
#nice, that shows that model 4, m4 and its MSE. 


#next task in the assingment is to create hypothetical values
# and then predict growth rate for those fake values. 
#pretty sure that's a bootstrap, right? 

fake_mushies <- data.frame(
  Temperature = c(20, 22, 25, 28),
  Humidity = c("Low", "Medium", "Medium", "High"),
  Nitrogen = c(10, 12, 14, 16),
  Light = c(8, 10, 12, 14)
)

fake_mushies$Humidity <- factor(fake_mushies$Humidity, levels = unique(mushies$Humidity))
#same thing as before, just use fake_mushies. 
fake_mushies$predict_GR <- predict(m4, newdata = fake_mushies)
fake_mushies

#now plot with our real values side by side with our bootstrapped ones 
ggplot() +
  geom_point(data = mushies, aes(x = Temperature, y = GrowthRate)) +
  geom_point(data = fake_mushies, aes(x = Temperature, y = predict_GR),
             color = "red", size = 3) +
  geom_line(data = fake_mushies, aes(x = Temperature, y = predict_GR),
            color = "red")+
  geom_smooth(method = "lm", se = TRUE)


nlr <- read.csv("non_linear_relationship.csv")

summary(nlr)
