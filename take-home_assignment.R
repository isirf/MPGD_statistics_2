# Statistics II - take-home assignment
# Isabell & Matheus, April 2022

# install/load all the needed packages (ggplot2 is included in tidyverse)
# "broom" is needed for the tidy function; "see" and "patchwork" are required by the "performance" package

install_github("easystats/performance")
pacman::p_load("tidyverse", "MASS", "ggpubr", "devtools", "performance", "broom", "see", "patchwork")


# 1 DISEASES & SURVEY WEIGHTS -----------------------------------------------------------------------------------------------------------------------------------------------

# # 1.3 Calculate the share of individuals with disease (variable disease dummy; 1 = “yes”; 0 = “no”),  -----------------------------------------------------------------------
# with and without survey weights. Provide an output table and briefly describe the differences.

# reading the required data file for exercise
data_disease = read.csv("/Volumes/GoogleDrive/My Drive/Stats2_take-home_assignment/data_disease.csv")
#data_disease = read.csv("/Users/isabell/GitHub/MPGD_statistics_II/data_disease.csv")

# computing the shares of sick and healthy individuals (disease_dummy == 0 and 1, respectively)
# table considering the observed frequency from the "disease_dummy" variable (without weights)
data_disease %>%
  group_by(disease_dummy) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# OPTIONAL: the following commands reproduce the previous table (without weights), but includes the "age_group" categories
data_disease %>%
  group_by(age_group, disease_dummy) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))

# table considering the observed frequency from the "disease_dummy" variable (with weights)
data_disease %>%
  group_by(disease_dummy) %>% 
  summarise(n = sum(survey_weight)) %>% 
  mutate(freq = n / sum(n))

# OPTIONAL: the following commands reproduce the previous table (with weights), but includes the "age_group" categories
data_disease %>%
  group_by(age_group, disease_dummy) %>% 
  summarise(n = sum(survey_weight)) %>% 
  mutate(freq = n / sum(n))



# 2 HEALTHCARE UTILISATION & COUNT MODELS -----------------------------------------------------------------------------------------------------------------------------------

# reading the required data file for exercise #2
data_healthcare = read.csv("/Volumes/GoogleDrive/My Drive/Stats2_take-home_assignment/data_healthcare.csv")
#data_healthcare = read.csv("/Users/isabell/GitHub/MPGD_statistics_II/data_healthcare.csv")


# 2.2 Explore the data I: Generate a histogram for the variable doctor visits. Interpret the histogram and  -----------------------------------------------------------------
# discuss which model should be used to explain the annual number of doctor visits and why.

ggplot() +
  geom_histogram(data = data_healthcare, 
                 aes(x = doctor_visits),
                 colour = "black", 
                 fill = "white", 
                 bins = 50) +
  labs(x = "Doctor Visits", 
       y = "Frequency",
       title = "Histogram - Doctor Visits",
       caption = "Source: own calculations") 

# compare mean and variance of doctor visits (Poisson distribution: mean and variance should be the same/very close)
mean(data_healthcare$doctor_visits)
var(data_healthcare$doctor_visits)


# 2.3 Method I: Estimate the effect of gender and country of residence on the number of annual doctor visits,  --------------------------------------------------------------
# using linear regression. Perform regression diagnostics and briefly interpret the residuals. 
# Was linear regression a good choice?

# linear regression model
LR_model = lm(doctor_visits ~ female + as.factor(country), data = data_healthcare)

# regression diagnostics
# residuals vs fitted shows that there is no relationship between residuals and predicted values
# normal Q-Q -> not a good fit - best case: values should be on the line or at least (very) near the line
plot(LR_model)


# 2.4 Method II: Estimate the effect of gender and country of residence on the number of annual doctor visits,  -------------------------------------------------------------
# using a Poisson model. Prepare an output table and interpret the results 
# (put special focus on the exact interpretation of the coefficients).

# poisson model
POI_model = glm(doctor_visits ~ female + as.factor(country), family = poisson, data = data_healthcare)

# regression diagnostics
# residuals vs fitted shows that there is no relationship between residuals and predicted values
# normal Q-Q -> not a good fit - best case: values should be on the line or at least (very) near the line 
# (not suited for Poisson)
plot(POI_model)


# 2.5 Method III: The distribution of the variable doctor visits shows signs of overdispersion,  ----------------------------------------------------------------------------
# i.e. its variance is much greater than it might be expected in a Poisson distribution. 
# Hence, also estimate a Negative Binomial model.

# negative binomial (glm.nb function from the "MASS" package)
NB_model = glm.nb(doctor_visits ~ female + as.factor(country), data = data_healthcare)

# regression diagnostics
# residuals vs fitted shows that there is no relationship between residuals and predicted values
# normal Q-Q -> not a good fit - best case: values should be on the line or at least (very) near the line 
# (not suited for Negative Binomial models)
plot(NB_model)


# # 2.6 Evaluate your results: Visually compare the observed and predicted values of doctor visits for the Poisson model  -----------------------------------------------------
# and the Negative Binomial model in two separate graphs. Which model has the better fit?

# add estimates from the three different models to data_healthcare
data_healthcare = data_healthcare %>%
  mutate(fitted_lm = fitted(LR_model),
         fitted_POI= fitted(POI_model),
         fitted_NB = fitted(NB_model))

# use summary to receive coefficient table for each model
summary(LR_model)
summary(POI_model)
summary(NB_model)

# tidy (from "broom" package) is another way to show the coefficients in a more ordered way
tidy(LR_model)
tidy(POI_model)
tidy(NB_model)

# scatter plot for linear regression with regression line
plot_linear = data_healthcare %>%
  ggplot(aes(x = fitted_lm, y = doctor_visits)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) +
  labs(x = "linear regression model",
       y = "doctor visits",
       title = "Fitted vs actual doctor visits - Linear Regression model",
       caption = "Source: own calculations") +
  xlim(0, 12) +
  ylim(0, 45) +
  geom_abline(intercept = 0, slope = 1, colour = "red")

# scatter plot for Poisson model with regression line
plot_poisson = data_healthcare %>%
  ggplot(aes(x = fitted_POI, y = doctor_visits)) +
  geom_point() +
  geom_smooth(method=glm, se=FALSE) +
  labs(x = "fitted poisson model",
       y = "doctor visits",
       title = "Fitted vs actual doctor visits - Poisson model",
       caption = "Source: own calculations") +
  xlim(0, 12) +
  ylim(0, 45) +
  geom_abline(intercept = 0, slope = 1, colour = "red")

# scatter plot for Negative Binominal model with regression line
plot_nb = data_healthcare %>%
  ggplot(aes(x = fitted_NB, y = doctor_visits)) +
  geom_point() +
  geom_smooth(method=glm.nb, se=FALSE) +
  labs(x = "fitted negative binominal model",
       y = "doctor visits",
       title = "Fitted vs actual doctor visits - Negative Binomial model",
       caption = "Source: own calculations") +
  xlim(0, 12) +
  ylim(0, 45) +
  geom_abline(intercept = 0, slope = 1, colour = "red")

ggarrange(plot_poisson, plot_nb, ncol = 2, nrow = 1)

# diagnostics checks for Poisson and Non Binomial models ("performance" package)
check_model(POI_model)
check_model(NB_model)
