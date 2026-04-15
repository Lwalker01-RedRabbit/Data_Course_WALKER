library(tidyverse)
library(broom)


salaries1995 <- read.csv("FacultySalaries_1995.csv")
View(salaries1995)

#Taking a look at the graph lets us know that "Rank" is our X variables. 
#Salary is our Y axis, along with a box plot range of numbers. Range is from 0 to 1000. 
# as a layer to this exam, each wrapped range is depended on rank. 


# Tidy the salary data
salary_tidy <- salaries1995 %>%
  select(State,
         Tier,
         AvgAssistProfSalary,
         AvgAssocProfSalary,
         AvgFullProfSalary) %>%
  pivot_longer(
    cols = c(AvgAssistProfSalary, AvgAssocProfSalary, AvgFullProfSalary),
    names_to = "Rank",
    values_to = "Salary") %>%
  mutate(Rank = recode(Rank,
                  "AvgAssistProfSalary" = "Assist",
                  "AvgAssocProfSalary"  = "Assoc",
                  "AvgFullProfSalary"   = "Full"),
    Rank = factor(Rank, levels = c("Assist", "Assoc", "Full")),
    Tier = factor(Tier, levels = c("I", "IIA", "IIB")),
    State = factor(State)) %>%
  filter(Tier %in% c("I", "IIA", "IIB"))

# Re-create boxplot
ggplot(salary_tidy, aes(x = Rank, y = Salary, fill = Rank)) +
  geom_boxplot() +
  facet_wrap(~ Tier) +
  labs(x = "Rank", y = "Salary", fill = "Rank") +
  scale_fill_manual(values = c("Assist" = "#F8766D",
                               "Assoc" = "#00BA38",
                               "Full"  = "#619CFF")) +
  theme_minimal(base_size = 16)



#put information into usable format for given plot.

#s1995long <- salaries1995 %>%
#  select(Tier, AvgAssistProfSalary, AvgAssocProfSalary, AvgFullProfSalary) %>%
#  pivot_longer(cols = c(AvgAssistProfSalary, AvgAssocProfSalary, AvgFullProfSalary),
#    names_to = "Rank",
#    values_to = "Salary") %>%
#  mutate(Rank = recode(Rank,
#                  "AvgAssistProfSalary" = "Assist",
#                  "AvgAssocProfSalary" = "Assoc",
#                  "AvgFullProfSalary" = "Full"))

#ggplot(s1995long, aes(x=Tier, y=Salary, fill = Tier)) +
#  geom_boxplot()+
#  facet_wrap(~Tier)+
#  labs(x = "Rank", y = "Salary")+
#  theme_dark()

#names(salaries1995)



#salaries_long <- salaries1995 %>%
#  select(Tier, AvgAssistProfSalary, AvgAssocProfSalary, AvgFullProfSalary) %>%
#  pivot_longer(cols = c(AvgAssistProfSalary, AvgAssocProfSalary, AvgFullProfSalary),
#    names_to = "Rank",
#    values_to = "Salary") %>%
#  mutate(Rank = recode(Rank,
#                  AvgAssistProfSalary = "Assist",
#                  AvgAssocProfSalary  = "Assoc",
#                  AvgFullProfSalary   = "Full"),
#    Rank = factor(Rank, levels = c("Assist", "Assoc", "Full")),
#    Tier = factor(Tier, levels = c("I", "IIA", "IIB",)))

#ggplot(salaries_long, aes(x = Rank, y = Salary, fill = Rank)) +
#  geom_boxplot() +
#  facet_wrap(~ Tier) +
#  labs(x = "Rank", y = "Salary") +
#  theme_dark()

# ANOVA  ------------------------------------------------------------------

#passing thru an aov into a model. 

# ANOVA model
anova_model <- aov(Salary ~ State + Tier + Rank, data = salary_tidy)

# Summary output
summary(anova_model)



# juniper -----------------------------------------------------------------


juni <- read.csv("Juniper_Oils.csv")
View(juni)

#copy paste chems from 
chems <- c("alpha.pinene","para.cymene","alpha.terpineol","cedr.9.ene","alpha.cedrene","beta.cedrene","cis.thujopsene","alpha.himachalene","beta.chamigrene","cuparene","compound.1","alpha.chamigrene","widdrol","cedrol","beta.acorenol","alpha.acorenol","gamma.eudesmol","beta.eudesmol","alpha.eudesmol","cedr.8.en.13.ol","cedr.8.en.15.ol","compound.2","thujopsenal")

juniper_tidy <- juni %>%
  select(YearsSinceBurn, all_of(chems)) %>%
  pivot_longer(cols = all_of(chems),
    names_to = "ChemicalID",
    values_to = "Concentration")

# Plot
ggplot(juniper_tidy, aes(x = YearsSinceBurn, y = Concentration, group = 1)) +
  geom_smooth(method = "loess", se = TRUE, color = "#2C6BFF", linewidth = 1.2) +
  facet_wrap(~ ChemicalID, scales = "free_y") +
  labs(x = "YearsSinceBurn", y = "Concentration") +
  theme_gray(base_size = 12) +
  theme(
    strip.text = element_text(size = 12),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 10))

juniper_tidy$ChemicalID <- factor(juniper_tidy$ChemicalID, levels = chems)

# Generalized linear model
chem_glm <- glm(
  Concentration ~ 0 + ChemicalID + ChemicalID:YearsSinceBurn,
  data = juniper_tidy,
  family = gaussian()
)

# Tidy output and keep only significant terms
sig_burn_terms <- tidy(chem_glm) %>%
  filter(str_detect(term, "YearsSinceBurn")) %>%
  filter(p.value < 0.05) %>%
  mutate(
    term = str_remove(term, "^ChemicalID"),
    term = str_replace(term, ":YearsSinceBurn", "")
  )

sig_burn_terms
