#install.packages("gganimate")
library(tidyverse)
library(gganimate)


dat <- read.csv("../../Data/BioLog_Plate_Data.csv") %>%
  as_tibble()

glimpse(dat)
colnames(dat)

tidy_dat <- dat %>%
  pivot_longer(
    cols = starts_with("Hr_"),
    names_to = "Time",
    values_to = "Absorbance"
  ) %>%
  #separate(Sample, into = c("Sample_ID", "Dilution", "Time"), sep = "_")
  mutate(
    Time = as.numeric(str_remove(Time, "Hr_")), 
    Absorbance = as.numeric(Absorbance),
    Dilution = as.numeric(Dilution),
    Sample_Type = case_when(
      grepl("Soil1", Sample.ID) ~ "Soil 1",
      grepl("Soil2", Sample.ID) ~ "Soil 2",
      grepl("Waste", Sample.ID) ~ "Waste_Water",
      grepl("ClearCreek", Sample.ID) ~ "ClearCreek",
      TRUE ~ Sample.ID
    )
  ) %>%
  filter(!is.na(Absorbance))

mean_dat <- tidy_dat %>%
  group_by(Sample_Type, Dilution, Time) %>%
  summarize(Mean_Absorbance = mean(Absorbance, na.rm = TRUE), .groups = "drop")

plot_dat <- mean_dat %>%
  filter(Dilution %in% c(0.001, 0.01, 0.1))


# plot --------------------------------------------------------------------


#tidy_dat %>%
#  filter(Dilution == 0.1) %>%
 # ggplot(aes(x = Time, y = Absorbance, color = Sample_Type)) +
  #stat_summary(fun = mean, geom = "line", linewidth = 1) +
  #stat_summary(fun = mean, geom = "point") +
  #facet_wrap(~Sample.ID) +
  #labs(title = "BioLog Absorb over time, Dilution = 0.1",
   #    x = "time in hours",
  #     y = "mean absorbance") +
  #theme_dark()

#i_dat <- tidy_dat %>%
 # filter(Substrate == "Itaconic Acid") %>%
  #group_by(Sample_Type, Time) %>%
#  summarize(Mean_Absorbance = mean(Absorbance, na.rm = TRUE)) %>%
#  filter(!is.na(Mean_Absorbance))


# animate -----------------------------------------------------------------

#i_anim <- tidy_dat %>%
#  filter(Substrate == "Itaconic Acid") %>%
#  group_by(Sample.ID, Sample_Type, Time) %>%    # mean of 3 replicates per sample
#  summarize(Mean_Absorbance = mean(Absorbance, na.rm = TRUE)) %>%
#  ungroup()

p_anim <- ggplot(plot_dat, aes(x = Time, y = Mean_Absorbance, color = Sample_Type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Absorbance over time by dilution",
    x = "Time hours",
    y = "Mean absorbance",
    color = "Sample"
  ) +
  facet_wrap(~Dilution, scales = "free_y", ncol = 1) +
  theme_minimal() +
  transition_reveal(Time)


animate(p_anim, nframes = 100, fps = 10, renderer = gifski_renderer("Itaconic_Animation.gif"))





