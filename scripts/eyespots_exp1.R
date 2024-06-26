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
# Design 1 (blank), 2(1 eyespot), 3(2 eyespots - control)

# The level of predation between the two locations will be the same
# The level of predation will increase with time
# Design 1 will be predated the most and design 3 will be predated the least


#_________________________----
# IMPORT DATA ----

eyespots_filtered <- read_csv("data/Experiment 1 edited.csv")

# filtered data set has NA's removed from the data set

#_________________________----
# CHECK DATA ----

# call the dataframe

eyespots_filtered

# check structure of data

str(eyespots_filtered)

#_________________________----
# TIDY DATA ----

# check data is in a tidy format
head(eyespots_filtered)

# check variable names
colnames(eyespots_filtered)

eyespots_filtered <- rename(eyespots_filtered,
                            "predation"="predated",  
)

eyespots_filtered <- rename(eyespots_filtered,
                            "day_of_the_exp"="day of the exp",  
)

# check for duplication
eyespots_filtered %>% 
  duplicated() %>% 
  sum()


eyespots_filtered$day_of_the_exp <- as_factor(eyespots_filtered$day_of_the_exp)
eyespots_filtered$design <- as_factor(eyespots_filtered$design)
eyespots_filtered$predation <- as_factor(eyespots_filtered$predation)
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
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))

# can see that design 1 was the most predated, and design 3 was predated the least
# but only differed from design 2 by 12.
# Design 1 had 81% chance of predation, 2 had a 59% chance and 3 had a 55% chance
# Can see number of missing dummies for each design

eyespots_filtered %>% 
  group_by(location, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))

#Can see that location 2 had a higher level of predation (72%) than location 1 (60%)

eyespots_predated <- filter(.data = eyespots_filtered, predation == "1")
eyespots_location1 <- filter(.data = eyespots_filtered, location == "1")
eyespots_location2 <- filter(.data = eyespots_filtered, location == "2")

head(eyespots_location1)
head(eyespots_location2)

eyespots_location1$predation <- as.numeric(as.character(eyespots_location1$predation))
eyespots_location2$predation <- as.numeric(as.character(eyespots_location2$predation))

eyespots_location1 %>% 
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
#predation rates: 1 = 80%, 2 = 53%, 3 = 48%

eyespots_location2 %>% 
  group_by(design, predation) %>% 
  summarise(n = n()) %>%
  mutate(prob_obs = n/sum(n))
# 1 = 83%, 2 = 68%, 3 = 65%

#_________________________----
# PLOTS ----

eyespots_filtered %>% 
  ggplot(aes(x=design,y=predation, fill=predation))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()
# Can see a larger predation rate and sample size for design 1, not much difference
# between design 2 and 3

eyespots_filtered %>% 
  ggplot(aes(x=location,y=predation, fill=predation))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()

# Can see that location 1 has a lower predation rate but a larger sample size than location 2,
# which again needs to be mentioned in discussion

eyespots_location1p <- filter(.data = eyespots_predated, location == "1")
eyespots_location2p <- filter(.data = eyespots_predated, location == "2")

eyespots_location1 %>% 
  ggplot(aes(x=day_of_the_exp,y=predation))+
  geom_bar(stat = "identity")

# not a clear pattern of change in predation levels at location 1


eyespots_location2 %>% 
  ggplot(aes(x=day_of_the_exp,y=predation))+
  geom_bar(stat = "identity")
# maybe use point with se
# clear increase in predation levels over the experiment at location 2 - indicating
# a stronger effect of learning

eyespots_filtered %>% 
  ggplot(aes(x=weather,y=predation, fill=predation))+
  geom_bar(position="stack", stat="identity")+
  coord_flip()
# shows predation/survival rate during each type of weather - due to the small sample size,
# I don't think this is that useful. 


#________________________----
# MODEL ----

ef_numcol <- eyespots_filtered %>% mutate(day_of_the_exp = as.numeric(day_of_the_exp))
# converting collection column into numeric data type as its more suitable for model


eyespots_model0 <- glm(predation~1, data = eyespots_filtered,
                       family = "binomial"(link=logit))

summary(eyespots_model0)

# null model which will be used to compare AIC scores with more complex models
# AIC = 1359.4, df = 1050 (remember AIC will decrease with increase in df)

eyespots_model1 <- glm(predation~design, data = eyespots_filtered,
                       family = "binomial"(link=logit))

summary(eyespots_model1)
# AIC decreases to 1300.7

eyespots_model2 <- glm(predation~design+day_of_the_exp+location+temperature, data = ef_numcol,
                       family = "binomial"(link=logit))
# date is not used because it correlates with collection which is included instead
# removed weather from model because of its vif of 25 and beacause in this data it does not,
# provide practical information

summary(eyespots_model2)


vif(eyespots_model2)
#test without interaction first

eyespots_model3 <- glm(predation~day_of_the_exp*design+design+day_of_the_exp+location+temperature, data = ef_numcol,
                       family = "binomial"(link=logit))
summary(eyespots_model3)
broom::tidy(eyespots_model3, conf.int=T)
# Does not seem to be an interaction between day_of_the_exp and design

drop1(eyespots_model3, test = "Chisq")

eyespotm3_des <- ggpredict(eyespots_model3, terms = c("day_of_the_exp","design"))

plot(eyespotm3_des)
# Shows that learning has the same effect across each design

vif(eyespots_model3)
# should get rid of interaction term between day_of_the_exp and design

eyespots_model4 <- glm(predation~day_of_the_exp*location+design+day_of_the_exp+location+temperature, data = ef_numcol,
                       family = "binomial"(link=logit))
#1191/1044 = 1.14 which suggests overdispersion
summary(eyespots_model4)
# collection loses its power

eyespotm4_loc <- ggpredict(eyespots_model4, terms = c("day_of_the_exp","location"))

plot(eyespotm4_loc)
# Does show a difference in learning between the two locations

vif(eyespots_model4)

drop1(eyespots_model4, test="Chisq")
broom::tidy(eyespots_model4, conf.int=T)

performance::check_model(eyespots_model4)
performance::check_model(eyespots_model4, check = "binned_residuals")
# some overdispersion

eyespots_model4ql <- glm(predation~day_of_the_exp*location+design+day_of_the_exp+location+temperature, data = ef_numcol,
                         family = "quasibinomial"(link=logit))
summary(eyespots_model4ql)
# adjusts for overdispersion and significance levels remain the same
broom::tidy(eyespots_model4ql, conf.int=T)

drop1(eyespots_model4ql, test="Chisq")
# overall effect of design on predation is significant 

performance::check_model(eyespots_model4ql, check = "binned_residuals")

exp1_model <- eyespots_model4ql %>% broom::tidy(conf.int = T) %>% 
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


locdes_data <- ggpredict(eyespots_model4ql, terms = c("location", "design"))
locdes_plot <- plot(locdes_data)
# shows predation levels of each design at each location


temp_data <- ggpredict(eyespots_model4ql, terms = c("temperature"))

temp_plot<- plot(temp_data, show_title = FALSE)
# negative relationship


# creating a model for each location

df_location1 <- filter(.data = ef_numcol, location == "1")
df_location2 <- filter(.data = ef_numcol, location == "2")


model_loc1 <- glm(predation~day_of_the_exp+design+temperature, data = df_location1,
                  family = "quasibinomial"(link=logit))
summary(model_loc1)

vif(model_loc1)

# day_of_the_exp has lost its power as expected

performance::check_model(model_loc1, check = "binned_residuals")

ml1_temp <- ggpredict(model_loc1, terms = c("temperature"))

plot(ml1_temp)

ml1_coldes <- ggpredict(model_loc1, terms = c("day_of_the_exp","design"))

plot(ml1_coldes)

model_loc2 <- glm(predation~day_of_the_exp+design+temperature, data = df_location2,
                  family = "binomial"(link=logit))
summary(model_loc2)
# day_of_the_exp significant as expected

vif(model_loc2)

performance::check_model(model_loc2, check = "binned_residuals")

ml2_temp <- ggpredict(model_loc2, terms = c("temperature"))

plot(ml2_temp)

ml2_coldes <- ggpredict(model_loc2, terms = c("day_of_the_exp","design"))

plot(ml2_coldes)

#________________________----
# PREDICTIONS ----

emmeans::emmeans(eyespots_model4ql, specs=~location, type="response")

design_tibble <- emmeans::emmeans(eyespots_model4ql, specs=~design, type="response") %>% as_tibble()
design_table <- design_tibble %>% select(- `df`) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  kbl(col.names = c("Design",
                    "Probability",
                    "SE",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Mean Probability of Predation For Each Design", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=12, position = "left")

# post-hoc pairwise comparisons between designs
emmeans::emmeans(eyespots_model4ql, specs = pairwise ~ design, type='response')
# no significant difference between designs 2 and 3

loc_tibble <- emmeans::emmeans(eyespots_model4ql, specs=~location, type = 'response') %>% as_tibble()

augment_glm <- function(mod, predict = NULL){
  fam <- family(mod)
  ilink <- fam$linkinv
  
  broom::augment(mod, newdata = predict, se_fit=T)%>%
    mutate(.lower = ilink(.fitted - 1.96*.se.fit),
           .upper = ilink(.fitted + 1.96*.se.fit), 
           .fitted=ilink(.fitted))
}

augment_glm(eyespots_model4ql)

augmented_data <- augment_glm(eyespots_model4ql)

# Filter data by design type
design1_data <- filter(augmented_data, design == 1)
design2_data <- filter(augmented_data, design == 2)
design3_data <- filter(augmented_data, design == 3)

# Plot predicted probabilities for each design type
combined_design_plot <- ggplot() +
  geom_line(data = design1_data, aes(x = day_of_the_exp, y = .fitted), color = "blue") +
  geom_ribbon(data = design1_data, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "blue") +
  geom_line(data = design2_data, aes(x = day_of_the_exp, y = .fitted), color = "red") +
  geom_ribbon(data = design2_data, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "red") +
  geom_line(data = design3_data, aes(x = day_of_the_exp, y = .fitted), color = "green") +
  geom_ribbon(data = design3_data, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "green") +
  labs(x = "Day of the experiment", y = "Probability of Predation", color = "Design Type") +
  scale_color_manual(values = c("blue", "red", "green"))+
  theme_minimal()


augmented_loc1 <- augment_glm(model_loc1)

# Filter data by design type
design1_loc1 <- filter(augmented_loc1, design == 1)
design2_loc1 <- filter(augmented_loc1, design == 2)
design3_loc1 <- filter(augmented_loc1, design == 3)

# Plot predicted probabilities for each design type
ggplot() +
  geom_line(data = design1_loc1, aes(x = day_of_the_exp, y = .fitted), color = "blue") +
  geom_ribbon(data = design1_loc1, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "blue") +
  geom_line(data = design2_loc1, aes(x = day_of_the_exp, y = .fitted), color = "red") +
  geom_ribbon(data = design2_loc1, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "red") +
  geom_line(data = design3_loc1, aes(x = day_of_the_exp, y = .fitted), color = "green") +
  geom_ribbon(data = design3_loc1, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "green") +
  labs(x = "day of the exp", y = "Probability of Predation", color = "Design Type") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()


augmented_loc2 <- augment_glm(model_loc2)

# Filter data by design type
design1_loc2 <- filter(augmented_loc2, design == 1)
design2_loc2 <- filter(augmented_loc2, design == 2)
design3_loc2 <- filter(augmented_loc2, design == 3)

# Plot predicted probabilities for each design type
ggplot() +
  geom_line(data = design1_loc2, aes(x = day_of_the_exp, y = .fitted), color = "blue") +
  geom_ribbon(data = design1_loc2, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "blue") +
  geom_line(data = design2_loc2, aes(x = day_of_the_exp, y = .fitted), color = "red") +
  geom_ribbon(data = design2_loc2, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "red") +
  geom_line(data = design3_loc2, aes(x = day_of_the_exp, y = .fitted), color = "green") +
  geom_ribbon(data = design3_loc2, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "green") +
  labs(x = "day of the exp", y = "Probability of Predation", color = "Design Type") +
  scale_color_manual(values = c("blue", "red", "green")) +
  theme_minimal()


# same plots but using eyespot_model 4 (with interaction) instead

# Filter data by location
augdata_loc1 <- filter(augmented_data, location == 1)
augdata_loc2 <- filter(augmented_data, location == 2)

design1_loc1a <- filter(augdata_loc1, design == 1)
design2_loc1a <- filter(augdata_loc1, design == 2)
design3_loc1a <- filter(augdata_loc1, design == 3)

# Plot predicted probabilities for each design type
loc1_learning_plot <- ggplot() +
  geom_line(data = design1_loc1a, aes(x = day_of_the_exp, y = .fitted), color = "blue") +
  geom_ribbon(data = design1_loc1a, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "blue") +
  geom_line(data = design2_loc1a, aes(x = day_of_the_exp, y = .fitted), color = "red") +
  geom_ribbon(data = design2_loc1a, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "red") +
  geom_line(data = design3_loc1a, aes(x = day_of_the_exp, y = .fitted), color = "green") +
  geom_ribbon(data = design3_loc1a, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "green") +
  ggtitle(label = "Predation through time at location 1")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Day of the experiment", y = "Probability of Predation", color = "Design Type") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_y_continuous(breaks=c(0.2,0.4, 0.6, 0.8, 1.0), limits = c(0, 1)) +
  scale_x_continuous(breaks=c(1,3,5,7,9)) +
  theme_bw(base_size = 15)

# Filter data by design type
design1_loc2a <- filter(augdata_loc2, design == 1)
design2_loc2a <- filter(augdata_loc2, design == 2)
design3_loc2a <- filter(augdata_loc2, design == 3)

# Plot predicted probabilities for each design type
loc2_learning_plot <- ggplot() +
  geom_line(data = design1_loc2a, aes(x = day_of_the_exp, y = .fitted), color = "blue") +
  geom_ribbon(data = design1_loc2a, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "blue") +
  geom_line(data = design2_loc2a, aes(x = day_of_the_exp, y = .fitted), color = "red") +
  geom_ribbon(data = design2_loc2a, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "red") +
  geom_line(data = design3_loc2a, aes(x = day_of_the_exp, y = .fitted), color = "green") +
  geom_ribbon(data = design3_loc2a, aes(x = day_of_the_exp, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "green") +
  ggtitle(label = "Predation through time at location 2")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Day of the experiment", y = "Probability of Predation", color = "Design Type") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_y_continuous(breaks=c(0.2,0.4, 0.6, 0.8, 1.0), limits = c(0, 1))+
  theme_bw(base_size = 15)


learning_plot <- (loc1_learning_plot+loc2_learning_plot)+
  plot_layout(guides = "collect") 


plot <- ggplot(design_tibble, aes(x = design, y = prob)) +
  # Add points
  geom_point() +
  # Add error bars
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),colour = "purple", width = 0.2) +
  # Add vertical line segments for the error bars
  geom_segment(aes(xend = design, yend = asymp.LCL),colour = "purple", linetype = "solid") +
  geom_segment(aes(xend = design, yend = asymp.UCL),colour = "purple", linetype = "solid") +
  # Add labels for mean
  geom_text(aes(label = sprintf("%.2f", prob), y = prob), hjust = 1.2) +
  ggtitle(label = "Impact of eyespot number on predation")+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(x = "Design", y = "Probability of predation") +
  scale_y_continuous(breaks = c(0.5, 0.6, 0.7, 0.8, 0.9), limits = c(0.5, 0.9))+
  scale_x_discrete(labels = c("No eyespots", "One eyespot", "Two eyespots"))+
  theme_bw(base_size = 15) 


# Print the plot
des_plot <- print(plot)

plot4 <- ggplot(loc_tibble, aes(x = location, y = prob)) +
  # Add points
  geom_point() +
  # Add error bars
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),colour = "purple", width = 0.2) +
  # Add vertical line segments for the error bars
  geom_segment(aes(xend = location, yend = asymp.LCL),colour = "purple", linetype = "solid") +
  geom_segment(aes(xend = location, yend = asymp.UCL),colour = "purple", linetype = "solid") +
  ggtitle(label = "Mean predation at each location in experiment 1")+
  theme(plot.title = element_text(hjust = 0.5))+
  # Add labels for means
  geom_text(aes(label = sprintf("%.2f", prob), y = prob), hjust = 1.2) +
  scale_y_continuous(breaks = c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9), limits = c(0.4, 0.9))+
  labs(x = "Location", y = "Probability of predation") +
  theme_bw(base_size = 15) 


# Print the plot
loc_exp1 <- print(plot4)



  

