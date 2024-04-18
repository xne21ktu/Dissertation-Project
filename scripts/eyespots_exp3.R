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

grouped5 <- aggregate(predation ~ days, data = eyespots3, FUN = sum)

ggplot(grouped5, aes(x = days, y = predation)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks=c(1, 2, 3))+
  theme_minimal()
#again an increase in predation with time into the experiment

# MODEL ----
#________________________----

eyespots3_model0 <- glm(predation~1, data = eyespots3,
                        family = "binomial"(link=logit))

summary(eyespots3_model0)

# null model which will be used to compare AIC scores with more complex models
# AIC = 1452.3, df = 1051

eyespots3_model1 <- glm(predation~design, data = eyespots3,
                        family = "binomial"(link=logit))

summary(eyespots3_model1)
#significantly higher level of predation for control 

eyespots3_model2 <- glm(predation~design+days+temperature+weather, data = eyespots3,
                        family = "binomial"(link=logit))
# date is not used because it correlates with collection which is included instead

summary(eyespots3_model2)
vif(eyespots3_model2)
drop1(eyespots3_model2, test="Chisq") 

#remove temperature and weather from model

eyespots3_model3 <- glm(predation~design+days, data = eyespots3,
                        family = "quasibinomial"(link=logit))
summary(eyespots3_model3)   
performance::check_model(eyespots3_model3, check = "binned_residuals")

broom::tidy(eyespots3_model3, conf.int=T)
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

#________________________----
# PREDICTIONS ----

exp3des_data <- ggpredict(eyespots3_model3, terms = c("design"))
exp3des_plot <- plot(exp3des_data)

design_tibble3 <- emmeans::emmeans(eyespots3_model3, specs=~design, type="response") %>% as_tibble()
design_table3 <- design_tibble3 %>% select(- `df`) %>% 
  mutate_if(is.numeric, round, 3) %>% 
  kbl(col.names = c("Design",
                    "Probability",
                    "SE",
                    "Lower 95% CI",
                    "Upper 95% CI"),
      caption = "Mean Probability of Predation For Each Design", 
      booktabs = T) %>% 
  kable_styling(full_width = FALSE, font_size=12, position = "left")

plot3 <- ggplot(design_tibble3, aes(x = design, y = prob)) +
  # Add points
  geom_point() +
  # Add error bars
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.2) +
  # Add vertical line segments for the error bars
  geom_segment(aes(xend = design, yend = asymp.LCL), linetype = "dotted") +
  geom_segment(aes(xend = design, yend = asymp.UCL), linetype = "dotted") +
  # Add labels for error bars
  geom_text(aes(label = sprintf("%.3f", asymp.UCL), y = asymp.UCL), vjust = -0.5) +
  geom_text(aes(label = sprintf("%.3f", asymp.LCL), y = asymp.LCL), vjust = 1.5) +
  # Customize plot aesthetics
  labs(x = "Design", y = "Probability") +
  theme_minimal() 


# Print the plot
des_plot3 <- print(plot3)


augment_glm <- function(mod, predict = NULL){
  fam <- family(mod)
  ilink <- fam$linkinv
  
  broom::augment(mod, newdata = predict, se_fit=T)%>%
    mutate(.lower = ilink(.fitted - 1.96*.se.fit),
           .upper = ilink(.fitted + 1.96*.se.fit), 
           .fitted=ilink(.fitted))
}


augmented_data3 <- augment_glm(eyespots3_model3)

# filter by design
design1_exp3 <- filter(augmented_data3, design == 1)
design2_exp3 <- filter(augmented_data3, design == 2)
design3_exp3 <- filter(augmented_data3, design == 3)

# Plot predicted probabilities for each design type
learning_plot3 <- ggplot() +
  geom_line(data = design1_exp3, aes(x = days, y = .fitted), color = "blue") +
  geom_ribbon(data = design1_exp3, aes(x = days, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "blue") +
  geom_line(data = design2_exp3, aes(x = days, y = .fitted), color = "red") +
  geom_ribbon(data = design2_exp3, aes(x = days, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "red") +
  geom_line(data = design3_exp3, aes(x = days, y = .fitted), color = "green") +
  geom_ribbon(data = design3_exp3, aes(x = days, ymin = .lower, ymax = .upper), alpha = 0.2, fill = "green") +
  labs(x = "Time (days)", y = "Probability of Predation", color = "Design Type") +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_y_continuous(breaks=c(0.2,0.4, 0.6, 0.8, 1.0), limits = c(0, 1)) +
  scale_x_continuous(breaks=c(1,2,3)) +
  theme_minimal()

