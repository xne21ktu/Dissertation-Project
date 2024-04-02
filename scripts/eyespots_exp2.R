#_________________________----
# PACKAGES ----
library(tidyverse) # tidy data packages.
library(kableExtra) # add ons for summary tables.
library(car) # for vif function - looks at correlations
library(ggeffects) # for ggpredict function to plot model
library(performance) # model assumption checking
library(patchwork) # combining plots
#_________________________----
# HYPOTHESES ----
# Design 1 (sparkle within eyespots), 2(sparkle outside of eyespots), 3(2 eyespots - control)
# Exposure time changed to 24 hours due to predation % increasing too much
# Location 2 will have a higher level of predation than location 1
# The level of predation will increase with time
# Design 1 will be predated the least and design 3 will be predated the most


#_________________________----
# IMPORT DATA ----

eyespots2 <- read_csv("data/Experiment 2 edited.csv")


# filtered data set has NA's removed from the data set

#_________________________----
# CHECK DATA ----

eyespots2 # call the dataframe

str(eyespots2) # check structure of data

#_________________________----
# TIDY DATA ----

# check data is in a tidy format
head(eyespots2)

# check variable names
colnames(eyespots2)

eyespots2 <- rename(eyespots2,
                            "predation"="predated",  
)

# check for duplication
eyespots2 %>% 
  duplicated() %>% 
  sum()



eyespots2$design <- as_factor(eyespots2$design)
eyespots2$predation <- as_factor(eyespots2$predation)
eyespots2$location <- as_factor(eyespots2$location)
eyespots2$weather <- as_factor(eyespots2$weather)
# converts selected columns into a factor so they can be used in analyses

summary(eyespots2)
# check for typos - by looking at impossible values
# quick summary

# missing values
eyespots2 %>%
  is.na() %>% 
  sum()

# NA = 0

eyespots2 %>% 
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
# Design 1 = 55.8% predation, design 2 = 61% predation, design 3 = 60.9% predation

eyespots2 %>% 
  group_by(location, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
# Location 1 - 49.4% predation, location 2 - 71.8% predation

eyespots2_location1 <- filter(.data = eyespots2, location == "1")
eyespots2_location2 <- filter(.data = eyespots2, location == "2")

eyespots2_location1 %>% 
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
#Predation at location 1: D1 = 43.2%, D2 = 51.8%, D3 = 52.8%
#clear that design 1 has less predation

eyespots2_location2 %>% 
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
#Predation at location 2: D1 = 71.7%, D2 = 72.5%, D3 = 71.2%
#very similar predation across all designs

#_________________________----
# PLOTS ----

eyespots2 %>% 
  ggplot(aes(x=design,y=predation, fill=predation))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()
#not much difference in sample size across designs
eyespots2 %>% 
  ggplot(aes(x=location,y=predation, fill=predation))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()

# Can see that location 1 has a lower predation rate but a larger sample size than location 2,
# which again needs to be mentioned in discussion
eyespots2_location1c <- eyespots2_location1 %>% mutate(collection = as.factor(collection))
eyespots2_location1c %>% 
  ggplot(aes(x=collection,y=predation))+
  geom_bar(stat = "identity")

eyespots2_location2c <- eyespots2_location2 %>% mutate(collection = as.factor(collection))
eyespots2_location2c %>% 
  ggplot(aes(x=collection,y=predation))+
  geom_bar(stat = "identity")

# both locations show an increase in predation from starting collection event

# MODEL ----
#________________________----

eyespots2_model0 <- glm(predation~1, data = eyespots2,
                       family = "binomial"(link=logit))

summary(eyespots2_model0)

# null model which will be used to compare AIC scores with more complex models
# AIC = 1399.6, df = 1033

eyespots2_model1 <- glm(predation~design, data = eyespots2,
                       family = "binomial"(link=logit))

summary(eyespots2_model1)
# AIC= 1401.1 - increases, suggesting design is not better explaining predation likelihood

eyespots2_model2 <- glm(predation~design+collection+location+temperature+weather, data = eyespots2,
                       family = "binomial"(link=logit))
# date is not used because it correlates with collection which is included instead

summary(eyespots2_model2)
vif(eyespots2_model2)
drop1(eyespots2_model2, test="Chisq")

#remove temperature and weather from model

eyespots2_model3ql <- glm(predation~design+collection+location, data = eyespots2,
                        family = "quasibinomial"(link=logit))
summary(eyespots2_model3ql)
# residual deviance/residual df = 1.2 which indicates overdispersion
performance::check_model(eyespots2_model3ql, check = "binned_residuals")
# does show some overdispersion = more variance than we expect from the prediction of the mean by our model.

drop1(eyespots2_model3ql, test="Chisq")
emmeans::emmeans(eyespots2_model3ql, specs= pairwise~design)



design_tibble <- emmeans::emmeans(eyespots2_model3ql, specs=~design, type="response") %>% as_tibble()
design_table <- design_tibble %>% select(- `df`) %>% 
  mutate_if(is.numeric, round, 4) %>% 
  kbl(col.names = c("Design",
                    "Probability",
                    "SE",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Mean Probability of Predation For Each Design", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=12, position = "left")

eyespots2_model4 <- glm(predation~design+collection+location+collection*location, data = eyespots2,
                        family = "binomial"(link=logit))
summary(eyespots2_model4)
drop1(eyespots2_model4, test="Chisq")

# does not seem to be an interaction between collection and location,
# suggesting learning is not different between locations
# Due to the significant relationship between collection and predation,
# learning does seem to be influencing predation

# creating a model for each location
# As seen earlier when calculating the % of predation for each design at each location,
# design 1 did show a decreased predation rate in location 1, so I am interested to see,
# if there is a significant difference between designs at this location
df2_location1 <- filter(.data = eyespots2, location == "1")
df2_location2 <- filter(.data = eyespots2, location == "2")


model2_loc1 <- glm(predation~collection+design, data = df2_location1,
                  family = "binomial"(link=logit))
summary(model2_loc1)
# does show significance for design 3 compared to design 1, and design 2 is not far off (P=0.058)
coldesl1_data <- ggpredict(model2_loc1, terms = c("collection", "design"))
plot(coldesl1_data)

model2_loc2 <- glm(predation~collection+design, data = df2_location2,
                   family = "binomial"(link=logit))
summary(model2_loc2)
# as expected there is no significant effect of design on predation

coldesl2_data <- ggpredict(model2_loc2, terms = c("collection", "design"))
plot(coldesl2_data)

# these differences suggest that the effect of design on predation may vary across different locations.

eyespots2_model5ql <- glm(predation~design+collection+location+design*location, data = eyespots2,
                        family = "quasibinomial"(link=logit))
summary(eyespots2_model5ql)
drop1(eyespots2_model5ql, test="Chisq")

#it appears that the effect of design on predation rates does not vary significantly between the two locations,
#despite differences in the significance of design in the individual models for each location. 
#drop1 also suggests removal of interaction term. Which is suprising,
# I assume that although there is a difference between design 1 and designs 2/3 at location 1,
# it is not big enough for there to be an interaction between location and design

emmeans::emmeans(eyespots2_model5ql, specs= pairwise~design|location)
# p value around 0.1 suggesting there may be something going on between design 1 and 2/3 and
# probabilities support this. No statistically significant evidence but suggest future work

