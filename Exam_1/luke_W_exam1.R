
# Exam Start, Luke Walker -------------------------------------------------


library(tidyverse)

covid_data_frame <- read_csv("cleaned_covid_data.csv")

glimpse(covid_data_frame)


# Get the letter A  -------------------------------------------------------


Letter_A_States <- covid_data_frame %>%
  filter(grepl("^A", Province_State))

unique(Letter_A_States$Province_State)
# alabama, alaska, arizona, arkansas 


# Plot deaths for A states ------------------------------------------------

Letter_A_States <- Letter_A_States %>%
  mutate(Last_Update = as.Date(Last_Update))

ggplot(Letter_A_States, aes(x = Last_Update, y = Deaths)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~Province_State, scales = "free") +
  labs(title = "Deaths over time for letter A states", 
       x = "date",
       y = "deaths") + 
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, hjust = 2))



# Plot peak fatality rate -------------------------------------------------


state_max_fatality_rate <- covid_data_frame %>%
  group_by(Province_State) %>%
  summarize(Maximum_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Maximum_Fatality_Ratio))

head(state_max_fatality_rate)

#New york, New Jersey, Massachusetts, Connecticut, new hampshire, pennsylvania
#we now have ratio numbers we can plot 

#order them first then plot 

#state_max_fatality_rates <- state_max_fatality_rate %>%
 # mutate(Province_State = factor(Province_State), levels = rev(Province_State))
state_max_fatality_rate <- state_max_fatality_rate %>%
  arrange(desc(Maximum_Fatality_Ratio)) %>%
  mutate(Province_State = factor(Province_State, levels = Province_State))

ggplot(state_max_fatality_rate, aes(x = Province_State, y = Maximum_Fatality_Ratio)) +
  geom_col(fill = "red") +
  labs(title = "Maximum Fatality rate by state",
       x = "State",
       y = "Max Fatality Ratio") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#theme(axis.text.x = element_text(angle = -90, hjust = 2))
#if you open the file up in zoom, it corrects itself


# bonus!!! ----------------------------------------------------------------


us_cumulative <- covid_data_frame %>%
  mutate(Last_Update = as.Date(Last_Update)) %>%
  group_by(Last_Update) %>%
  summarize(Total_Deaths = sum(Deaths, na.rm = TRUE))

ggplot(us_cumulative, aes(x = Last_Update, y = Total_Deaths)) +
  geom_line(color = "blue", size = 2) +
  labs(title = "Cumulative Covid deaths in the U.S.A.",
       x = "Date", 
       y = "Total Deaths") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, hjust = 2))


