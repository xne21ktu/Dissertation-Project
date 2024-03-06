#_________________________----
# PACKAGES ----
library(tidyverse) # tidy data packages.
library(kableExtra) # add ons for summary tables.
library(janitor)# clean variable names

#_________________________----
# HYPOTHESES ----

# Design 1 (blank), 2(1 eyespot), 3(2 eyespots - control)

# The level of predation between the two locations will be significantly different
# The level of predation will increase with time
# Design 1 will be predated the most and design 3 will be predated the least


#_________________________----
# IMPORT DATA ----

eyespots <- read_csv("data/Experiment 1.csv")
eyespots_filtered <- read_csv("data/Experiment 1 edited.csv")

#_________________________----
# CHECK DATA ----

eyespots # call the dataframe

str(eyespots) # check structure of data

eyespots <- clean_names(eyespots)

#_________________________----
# TIDY DATA ----

# check data is in a tidy format
head(eyespots)

# check variable names
colnames(eyespots)

# check for duplication
eyespots %>% 
  duplicated() %>% 
  sum()


eyespots$collection <- as_factor(eyespots$collection)
eyespots$design <- as_factor(eyespots$design)
eyespots$predated <- as_factor(eyespots$predated)
eyespots$location <- as_factor(eyespots$location)
eyespots$rain <- as_factor(eyespots$rain)
eyespots$overcast <- as_factor(eyespots$overcast)
eyespots$sun <- as_factor(eyespots$sun)
eyespots$slug <- as_factor(eyespots$slug)
# converts selected columns into a factor so they can be used in analyses

summary(eyespots)
# check for typos - by looking at impossible values
# quick summary

# missing values
eyespots %>%
  is.na() %>% 
  sum()
# 29 NA values, which represent missing dummies from the experiment

eyespots %>% 
  group_by(design, predated) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))

# can see that design 1 was the most predated, and design 3 was predated the least
# but only differed from design 2 by 12.
# Design 1 had 80% chance of predation, 2 had a 57% chance and 3 had a 54% chance
# Can see number of missing dummies for each design

eyespots %>% 
  group_by(location, predated) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))

#Can see that location 2 (69%) had a higher level of predation than location 1 (59%)


#_________________________----
# PLOTS ----

eyespots %>% 
  ggplot(aes(x=design, fill=predated))+
  geom_bar(position=position_dodge())+
  coord_flip()


#________________________----
# MODEL ----


  


