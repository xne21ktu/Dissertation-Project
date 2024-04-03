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
# Design 1 (UV-reflective sparkle), 2(UV-absorbant sparkle), 3(2 eyespots - control)
# Exposure time changed to 8 hours due to predation % increasing too much
# The level of predation will increase with time
# Design 1 will be predated the least, and design 3 will predated the most


#_________________________----
# IMPORT DATA ----

eyespots3 <- read_csv("data/Experiment 3 edited.csv")


# filtered data set has NA's removed from the data set

#_________________________----
# CHECK DATA ----

eyespots3 # call the dataframe

str(eyespots3) # check structure of data

#_________________________----
# TIDY DATA ----

# check data is in a tidy format
head(eyespots3)

# check variable names
colnames(eyespots3)

eyespots3 <- rename(eyespots3,
                    "predation"="predated",  
)

# check for duplication
eyespots3 %>% 
  duplicated() %>% 
  sum()



eyespots3$design <- as_factor(eyespots3$design)
eyespots3$weather <- as_factor(eyespots3$weather)
# converts selected columns into a factor so they can be used in analyses

summary(eyespots3)
# check for typos - by looking at impossible values
# quick summary

# missing values
eyespots3 %>%
  is.na() %>% 
  sum()

# NA = 0

eyespots3 %>% 
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
# Design 1 = 52.6% predation, design 2 = 49.1% predation, design 3 = 61.3% predation

#_________________________----
# PLOTS ----
eyespots3pnum <- eyespots3 %>% mutate(predation = as.factor(predation))
eyespots3pnum %>% 
  ggplot(aes(x=design,y=predation, fill=predation))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()
#not much difference in sample size across designs - matches data

eyespots3c <- eyespots3 %>% mutate(collection = as.factor(collection))
eyespots3c %>% 
  ggplot(aes(x=collection,y=predation))+
  geom_bar(stat = "identity")
#again an increase in predation with time into the experiment

# MODEL ----
#________________________----

eyespots3_model0 <- glm(predation~1, data = eyespots3,
                        family = "binomial"(link=logit))

summary(eyespots3_model0)

# null model which will be used to compare AIC scores with more complex models
# AIC = 1450.3, df = 1051

eyespots3_model1 <- glm(predation~design, data = eyespots3,
                        family = "binomial"(link=logit))

summary(eyespots3_model1)
#significantly higher level of predation for control 

eyespots3_model2 <- glm(predation~design+collection+temperature+weather, data = eyespots3,
                        family = "binomial"(link=logit))
# date is not used because it correlates with collection which is included instead

summary(eyespots3_model2)
vif(eyespots3_model2)
drop1(eyespots3_model2, test="Chisq") 

#remove temperature and weather from model

eyespots3_model3 <- glm(predation~design+collection, data = eyespots3,
                        family = "quasibinomial"(link=logit))
summary(eyespots3_model3)   
performance::check_model(eyespots3_model3, check = "binned_residuals")


drop1(eyespots3_model3, test="Chisq") 
emmeans::emmeans(eyespots3_model3, specs = pairwise ~ design, type = 'response')
# overall effect of design on predation is significant
# pairwise comparisons between each design

exp3_model <- eyespots3_model3 %>% broom::tidy(conf.int = T) %>% 
  select(-`std.error`) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  kbl(col.names = c("Predictors",
                    "Estimates",
                    "Z-value",
                    "P",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Linear model coefficients", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=16)

exp3des_data <- ggpredict(eyespots3_model3, terms = c("design"))
exp3des_plot <- plot(exp3des_data)
