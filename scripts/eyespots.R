#_________________________----
# PACKAGES ----
library(tidyverse) # tidy data packages.
library(kableExtra) # add ons for summary tables.
library(janitor)# clean variable names
library(emmeans) # conversion of log odds to probability
library(car)
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

# filtered data set has NA's removed from the data set

#_________________________----
# CHECK DATA ----

eyespots # call the dataframe

eyespots_filtered

str(eyespots) # check structure of data

str(eyespots_filtered)

#_________________________----
# TIDY DATA ----

# check data is in a tidy format
head(eyespots_filtered)

# check variable names
colnames(eyespots_filtered)

# check for duplication
eyespots_filtered %>% 
  duplicated() %>% 
  sum()


eyespots_filtered$collection <- as_factor(eyespots_filtered$collection)
eyespots_filtered$design <- as_factor(eyespots_filtered$design)
eyespots_filtered$predated <- as_factor(eyespots_filtered$predated)
eyespots_filtered$location <- as_factor(eyespots_filtered$location)
eyespots_filtered$weather <- as_factor(eyespots_filtered$weather)
eyespots_filtered$slug <- as_factor(eyespots_filtered$slug)
# converts selected columns into a factor so they can be used in analyses

summary(eyespots_filtered)
# check for typos - by looking at impossible values
# quick summary

# missing values
eyespots_filtered %>%
  is.na() %>% 
  sum()
# original (eyespots) contained 29 NA values, which represent missing dummies from the experiment and were removed
# so now NA numbers = 0

eyespots_filtered %>% 
  group_by(design, predated) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))

# can see that design 1 was the most predated, and design 3 was predated the least
# but only differed from design 2 by 12.
# Design 1 had 81% chance of predation, 2 had a 59% chance and 3 had a 55% chance
# Can see number of missing dummies for each design

eyespots_filtered %>% 
  group_by(location, predated) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))

#Can see that location 2 had a higher level of predation (72%) than location 1 (60%)


#_________________________----
# PLOTS ----

eyespots_filtered %>% 
  ggplot(aes(x=design, fill=predated))+
  geom_bar(position=position_dodge())+
  coord_flip()

eyespots_filtered %>% 
  ggplot(aes(x=design,y=predated, fill=predated))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()
# Can see a larger predation rate and sample size for design 1, not much difference
# between design 2 and 3

eyespots_filtered %>% 
  ggplot(aes(x=location,y=predated, fill=predated))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()

# Can see that location 1 has a lower predation rate but a larger sample size than location 2,
# which again needs to be mentioned in discussion

eyespots_filtered2 <- filter(.data = eyespots_filtered, predated == "1")
eyespots_location1 <- filter(.data = eyespots_filtered2, location == "1")
eyespots_location2 <- filter(.data = eyespots_filtered2, location == "2")

head(eyespots_location1)

eyespots_location1$predated <- as.numeric(as.character(eyespots_location1$predated))

eyespots_location1 %>% 
  ggplot(aes(x=collection,y=predated))+
  geom_bar(stat = "identity")

# not a clear pattern of change in predation levels at location 1,
# although the last collection shows the highest level of predation

eyespots_location2$predated <- as.numeric(as.character(eyespots_location2$predated))

  
eyespots_location2 %>% 
  ggplot(aes(x=collection,y=predated))+
  geom_bar(stat = "identity")
# maybe use point with se
# clear increase in predation levels over the experiment at location 2 - indicating
# a stronger effect of learning

eyespots_filtered %>% 
  ggplot(aes(x=weather,y=predated, fill=predated))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()
# shows predation/survival rate during each type of weather - due to the small sample size,
# I don't think this is that useful. 


#________________________----
# MODEL ----
# make a null model - compare AIC
eyespots_model <- glm(predated~design, data = eyespots_filtered,
                      family = "binomial"(link=logit))

summary(eyespots_model)

eyespots_model %>% 
  broom::tidy(conf.int=T)

emmeans::emmeans(eyespots_model, specs=~design, type="response")
ef <- eyespots_filtered %>% mutate(collection = as.numeric(collection))
eyespots_model2 <- glm(predated~collection+design+location+design+weather+temperature, data = ef,
                      family = "binomial"(link=logit))

summary(eyespots_model2)
eyespo2data <- ggpredict(eyespots_model2, terms = c("collection","design"))
plot(eyespo2data)
vif(eyespots_model2)
#test without interaction

# ggpredict to plot model instead of converting to prob
