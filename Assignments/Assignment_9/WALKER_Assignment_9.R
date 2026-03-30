library(tidyverse)
library(ggplot2)



#read data, then create readable HTML files -- which is a crucial skill for the final. 

grad <- read.csv("Data/GradSchool_Admissions.csv")

print(grad)
str(grad)
View(grad)
#working with 4 values, admit, gre, gpa, rank. 

#create a model to work with
mod1 <- glm(admit ~ gre + gpa + rank, data = grad, family = binomial)
summary(mod1)

mod2 <- glm(admit ~ gre + gpa, data = grad, family = binomial)
summary(mod2)

mod3 <- glm(admit ~ gpa + rank, data = grad, family = binomial)
summary(mod3)

mod4 <- glm(admit ~ gre + rank, data = grad, family = binomial)
summary(mod4)

#buncha random comparison models. Now compare! 

AIC(mod1, mod2, mod3, mod4)

#mod1 is the best model, considering it has all the pieces. 




