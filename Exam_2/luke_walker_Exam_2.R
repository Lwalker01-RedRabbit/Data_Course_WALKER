# EXAM 2 
#luke Walker 


#get csv file into a varible so I can manipulate it 

Under5deathrate <- read.csv("unicef-u5mr.csv")

#load all the library that I want to use
library(dbplyr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyverse)

#look at the data, clean it for missing values 
view(Under5deathrate)
str(Under5deathrate)
#per 1000 live births, this data is gloomy. 

#read these into a plot. I will have x for time value, and y for deaths under 5 per 1000.
#first the data must be in the correct format. 

ggplot(Under5deathrate, aes(x = Year, y = Rate))+
  geom_line()


Under5deathrate_time_x <- Under5deathrate %>%
  pivot_longer(
    col = starts_with("U5MR"),
    names_to = "Year",
    values_to = "Rate"
  )%>%
  mutate(Year = as.numeric(sub("U5MR\\.", "", Year)))



ggplot(Under5deathrate_time_x, aes(x = Year, y = Rate,))+
  geom_line()

ggplot(Under5deathrate_time_x, aes(x= Year, y = Rate, group = CountryName))+
  geom_line()+
  facet_wrap(~ Continent)+
  scale_x_continuous(breaks = seq(1950, 2020, by = 20))+
  labs(x = "Year", y = "Under 5 mortality")



# for the 2nd plot I see on the left side is the mean of Under 5 mortality. 


UN5mean_data <- Under5deathrate_time_x %>%
  group_by(Continent, Year) %>%
  summarise(Mean_U5MR = mean(Rate, na.rm = TRUE))

ggplot(UN5mean_data, aes(x = Year, y = Mean_U5MR, color = Continent))+
  geom_line()+
  labs(x= "Year", y= "Mean U5MR")+
  scale_x_continuous(breaks = seq(1950, 2020, by = 20))

#aight time to create 3 models. 
#first is year. 
modyear <- lm(Rate ~ Year, data = Under5deathrate_time_x)

#now year + continent 
modcon <- lm(Rate ~ Year+Continent, data = Under5deathrate_time_x)

#and now do it by interaction between time and continent
modyear_times_con <- lm(Rate ~ Year*Continent, data = Under5deathrate_time_x)


#compare mods 
summary(modyear)
summary(modcon)
summary(modyear_times_con)


#let's add the 3 model predictions to a data set
#we'll use a pipeline and clean up any missing data so our models fit nice. 
model_data <- Under5deathrate_time_x %>% 
  filter(!is.na(Rate), !is.na(Year), !is.na(Continent))
#refit models. 
mod1 <- lm(Rate ~ Year, data = model_data)
mod2 <- lm(Rate ~ Year+Continent, data = model_data)
mod3 <- lm(Rate ~ Year*Continent, data = model_data)

model_data <- model_data %>%
  mutate(pred1 = predict(mod1),
         pred2 = predict(mod2),
         pred3 = predict(mod3))

pred <- model_data %>%
  pivot_longer(cols = starts_with("pred"),
               names_to = "model",
               values_to = "predicted")


ggplot(pred, aes(x = Year, y = predicted, color = Continent))+
  geom_line()+
  facet_wrap(~ model)







