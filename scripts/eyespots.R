#_________________________----
# PACKAGES ----
library(tidyverse) # tidy data packages.
library(kableExtra) # add ons for summary tables.
library(janitor)# clean variable names
library(car) # for vif function - looks at correlations
library(ggeffects) # for ggpredict function to plot model
library(performance) # model assumption checking
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

eyespots_predated <- filter(.data = eyespots_filtered, predated == "1")
eyespots_location1 <- filter(.data = eyespots_filtered, location == "1")
eyespots_location2 <- filter(.data = eyespots_filtered, location == "2")

head(eyespots_location1)
head(eyespots_location2)

eyespots_location1$predated <- as.numeric(as.character(eyespots_location1$predated))
eyespots_location2$predated <- as.numeric(as.character(eyespots_location2$predated))

eyespots_location1 %>% 
  group_by(design, predated) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
#predation rates: 1 = 80%, 2 = 53%, 3 = 48%

eyespots_location2 %>% 
  group_by(design, predated) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
# 1 = 83%, 2 = 68%, 3 = 65%

#_________________________----
# PLOTS ----

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

eyespots_location1p <- filter(.data = eyespots_predated, location == "1")
eyespots_location2p <- filter(.data = eyespots_predated, location == "2")

eyespots_location1 %>% 
  ggplot(aes(x=collection,y=predated))+
  geom_bar(stat = "identity")

# not a clear pattern of change in predation levels at location 1,
# although the last collection shows the highest level of predation

  
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

ef_numcol <- eyespots_filtered %>% mutate(collection = as.numeric(collection))
# converting collection column into numeric data type as its more suitable for model


eyespots_model0 <- glm(predated~1, data = eyespots_filtered,
                      family = "binomial"(link=logit))

summary(eyespots_model0)

# null model which will be used to compare AIC scores with more complex models
# AIC = 1359.4, df = 1050 (remember AIC will decrease with increase in df)

eyespots_model1 <- glm(predated~design, data = eyespots_filtered,
                      family = "binomial"(link=logit))

summary(eyespots_model1)
# AIC decreases to 1300.7

eyespots_model2 <- glm(predated~collection+design+location+temperature, data = ef_numcol,
                      family = "binomial"(link=logit))

# date is not used because it correlates with collection which is included instead
# removed weather from model because of its vif of 25 and beacause in this data it does not,
# provide practical information

summary(eyespots_model2)


vif(eyespots_model2)
#test without interaction first

eyespots_model3 <- glm(predated~collection*design+design+collection+location+temperature, data = ef_numcol,
                      family = "binomial"(link=logit))
summary(eyespots_model3)
# Does not seem to be an interaction between collection and design

eyespotm3_des <- ggpredict(eyespots_model3, terms = c("collection","design"))

plot(eyespotm3_des)
# Shows that learning has the same effect across each design

vif(eyespots_model3)
# should get rid of interaction term between collection and design

eyespots_model4 <- glm(predated~collection*location+design+collection+location+temperature, data = ef_numcol,
                       family = "binomial"(link=logit))

summary(eyespots_model4)
# collection loses its power

eyespotm4_loc <- ggpredict(eyespots_model4, terms = c("collection","location"))

plot(eyespotm4_loc)
# Does show a difference in learning between the two locations

vif(eyespots_model4)

performance::check_model(eyespots_model4)
performance::check_model(eyespots_model4, check = "binned_residuals")


# creating a model for each location

df_location1 <- filter(.data = ef_numcol, location == "1")
df_location2 <- filter(.data = ef_numcol, location == "2")


model_loc1 <- glm(predated~collection+design+temperature, data = df_location1,
                  family = "binomial"(link=logit))
summary(model_loc1)

vif(model_loc1)

# collection has lost its power

performance::check_model(model_loc1, check = "binned_residuals")

ml1_temp <- ggpredict(model_loc1, terms = c("temperature"))

plot(ml1_temp)

ml1_coldes <- ggpredict(model_loc1, terms = c("collection","design"))

plot(ml1_coldes)

model_loc2 <- glm(predated~collection+design+temperature, data = df_location2,
                  family = "binomial"(link=logit))
summary(model_loc2)

vif(model_loc2)

performance::check_model(model_loc2, check = "binned_residuals")

ml2_temp <- ggpredict(model_loc2, terms = c("temperature"))

plot(ml2_temp)

ml2_coldes <- ggpredict(model_loc2, terms = c("collection","design"))

plot(ml2_coldes)

