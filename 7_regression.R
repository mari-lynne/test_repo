# Modelling in R ---------------------------------------------------------------

# Aims

# Evaluate linear and logistic regression models in R

# Packages ---------------------------------------------------------------------

# Load Packages
library(ggpubr)
library(ggplot2)
library(viridis)
library(ggvis)
library(stringr)
library(forcats)
library(janitor)
library(dplyr)
library(tidylog)
library(patchwork)  # Use install.packages("package_name") if not installed yet

# New packages
install.packages("corrplot")
install.packages("Hmisc")
install.packages("broom")
install.packages("car")

library(corrplot)
library(Hmisc)
library(broom)
library(car)


# Set Directories (file paths) =================================================
work_dir <- c("C:/Users/mjohnso5/Documents/teaching") # default folder for R to work/save in

in_dir <- "C:/Users/mjohnso5/Documents/teaching/data" # Where input data is saved 
plot_dir <- "C:/Users/mjohnso5/Documents/teaching/plots"
out_dir <- "C:/Users/mjohnso5/Documents/teaching/results" # folder you want to save results to

# Update these to the address (file path) of the folder where you saved YOUR data

setwd(work_dir) 

# Downloaded summary exposure value data from 
# https://vizhub.healthdata.org/gbd-results/

# Load and tidy data ===========================================================

enteric_data <- read.csv(file = "C:/Users/mjohnso5/Documents/teaching/data/tutorial_data/enteric_death_data.csv", stringsAsFactors = TRUE) # Change address to where you saved the data :)


# Check data: use table() for categorical vars, summary for continuous data
str(enteric_data)
table(enteric_data$location)
table(enteric_data$age)
table(enteric_data$year)
summary(enteric_data$enteric_deaths_pct)

enteric_data <- dplyr::distinct(enteric_data)


# Risk factors 
risks <- read.csv(file = "C:/Users/mjohnso5/Documents/teaching/data/tutorial_data/risk_factors.csv", stringsAsFactors = TRUE)

str(risks)

# remove upper lower CI col for now and other redundant columns
risks <- select(risks, -c("upper", "lower", "metric", "sex"))
risks <- distinct(risks)
table(risks$rei)

# Join data 

# Match using all the grouping variables, in this data set that is location, year, age
enteric_risks <- inner_join(enteric_data, risks, by = c("location", "age", "year"))

enteric_risks <- distinct(enteric_risks)

# inner join only keeps data with matches in both df's, left join would have kept all the data/rows from the left df - in this case enteric_data

# Check structure
str(enteric_risks)

# Quick look at unique REIs (risk factors)
levels(enteric_risks$rei)

# Check for missing values
summary(enteric_risks)

# Hypothesis exploring ==================================================================

# Example: Does the summary exposure value (SEV) for “Unsafe water source” predict the percentage of deaths due to enteric infections across regions?

# Linear Regression:
# y = b0 + b1x + e
# Enteric_Deaths = b0 + b Unsafe water source + e

# Multiple Regression:
# Enteric_Deaths = b0 + bUnsafe water source + Unsafe sanitation + Iron Deficiency + e

# Note: Regression assumes observations are independent
# So multiple observations from the same countries/age groups invalidates that assumption
# Therefore for the regression examples we will just look at one year across all ages
# Later will show how to deal with longitudinal data :) 


df <- enteric_risks %>%
  filter(age == "<5 years",
         year == "2010") %>%   # Pick a year and age group of interest
  select(location, year, enteric_deaths_pct, rei, val)

head(df)


###  Reshape data from long to wide --------------------------------------------

# In R regression, each predictor (independent variable) must be its own column in the data frame.
# This is because lm() (and other modeling functions) expect each column to represent a single numeric variable you can assign a coefficient to.


# 'rei' will become separate columns for each risk factor
library(tidyr)

df_multi <- df %>%
  pivot_wider(
    names_from = rei,
    values_from = val
  ) %>% # Pivot wide, then rename the new columns so no annoying spaces
  rename(
    SEV_unsafe_water = `Unsafe water source`,
    SEV_unsafe_sanitation = `Unsafe sanitation`,
    SEV_no_handwashing = `No access to handwashing facility`,
    SEV_iron_deficiency = `Iron deficiency`,
    SEV_VitA_deficiency = `Vitamin A deficiency`,
    SEV_Zinc_deficiency = `Zinc deficiency`,
    SEV_Growth = `Child growth failure`,
    SEV_low_temp = `Low temperature`,
    SEV_high_temp = `High temperature`
  ) #  SEV = summary exposure value

#  “Note: SEV values range from 0 to 100 and represent the summary exposure to the risk factor as a percentage of the theoretical minimum risk exposure distribution. - See GBD paper for more details on how calculated

# Inspect the reshaped data
head(df_multi)


# Explore variables/trends =====================================================


# Exploratory plots
ggplot(df_multi, aes(x = SEV_unsafe_water, y = enteric_deaths_pct, color = location)) +
  geom_point(size =4) +
  labs(y = "Enteric Deaths (%)") + theme_bw()


# This works well for exploring trends with a single predictor...
# But what if we want to look at more than one variable
# And how do we determine the effect size/significance of this trend

# Regression Model -------------------------------------------------------------

# Plan:
# What is our response variable (i.e y axis)
# What are our predictors (i.e x axis)

# Basic structure
model <- lm(outcome ~ predictor(s), data = your_dataframe)

# Let's examine unsafe water
# Single predictor = Univariate Regression
water_model <- lm(enteric_deaths_pct ~ SEV_unsafe_water, data = df_multi)
summary(water_model)

library(broom)
tidy(water_model)

# Check Assumptions -------------------------------------------------------------

plot(water_model)

# uses plot.lm() function
?plot.lm() # which argument decides which model plot to show

# Basic diagnostic plot
plot(water_model, which = 1)  # Residuals vs Fitted
plot(water_model, which = 2)  # QQ plot for normality of residuals


# Multivariate Linear Regression -----------------------------------------------

# Let's include more predictor variables in our model and see if it improves our estimates

# Explore multiple variables
# Loop thru plot code from earlier

# Loop through all variables of interest
names(df_multi)
vars <- colnames(df_multi[-c(1:2)], )
vars

for (i in vars) {
  plot <- ggplot(df_multi,
                 aes(x = .data[[i]], y = .data[["enteric_deaths_pct"]], color = location)) +
    geom_point(size = 3) +
    labs(y = "Enteric Deaths (%)")
  print(plot)
  Sys.sleep(0.5) # pause between plots
}

# Gives a quick scan of what variables might be useful to include

### Multicolinearity check -----------------------------------------------------

# Check for correlations between multiple predictor variables
  # I tend to do this assumption check beforehand so I can know what to include
  # But you can do it after (familiarity with the field/data helps here)

library(corrplot)
library(Hmisc)

# Make sure your predictors are numeric only
predictors <- df_multi %>%
  select(
    SEV_unsafe_water,
    SEV_unsafe_sanitation,
    SEV_iron_deficiency,
    SEV_VitA_deficiency,
    SEV_Zinc_deficiency,
    SEV_no_handwashing,
    SEV_Growth,
    SEV_high_temp,
    SEV_low_temp
  )

# Compute correlation coefficients and p-values
cor_results <- rcorr(as.matrix(predictors))

# Extract matrix of correlations and p-values
cor_matrix <- cor_results$r
p_matrix <- cor_results$P

?corrplot
corrplot(cor_matrix,
         method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         p.mat = p_matrix,
         sig.level = 0.05,
         insig = "pch",           # use symbols for non-significant
         pch.cex = 1.5,           # size of symbols
         pch.col = "black")       # color of symbols


# Fit multivariate linear regression model -------------------------------------

# Predicting enteric deaths (%) using multiple predictors

multi_model <- lm(
  enteric_deaths_pct ~ SEV_unsafe_water  + SEV_iron_deficiency + SEV_low_temp,
  data = df_multi
) # SEV_low_temp

# Summarize model results

summary(water_model)
summary(multi_model)
tidy(multi_model)

# Compare to just using water as a predictor
tidy(water_model)


glance(water_model)
glance(multi_model)

### Check model assumptions ----------------------------------------------------

plot(multi_model)

# uses plot.lm() function
?plot.lm() # which argument decides which model plot to show

# Basic diagnostic plot
plot(multi_model, which = 1)  # Residuals vs Fitted
plot(multi_model, which = 2)  # QQ plot for normality of residuals

# Exercise --------------------------------------------------------------------

# Using df_multi, explore two other factors and their relationship with enteric deaths
# Basic structure
model <- lm(outcome ~ predictor + extra vars, data = your_dataframe)

str(df_multi)
summary(your_model)
tidy()
glance()



# Logistic Regression ----------------------------------------------------------

# Example: Can we predict if a region will have a high or low burden of enteric deaths based on the level of unsafe water
#
# Logistic regression is appropriate because our outcome will be binary:
# 1 = High burden region
# 0 = Low burden region

# Note we are including multiple years here (just to increase our sample size, but not an ideal example to use sorry!)
str(enteric_risks)

df2 <- enteric_risks %>%
  filter(age == "<5 years") %>% # Pick an age group to focus on
  select(location, year, enteric_deaths_pct, rei, val)

df2 <- distinct(df2)

# Again reshape to wide format
df_multi2 <- df2 %>%
  pivot_wider(
    names_from = rei,
    values_from = val
  ) %>% # Pivot wide, then rename the new columns so no annoying spaces
  rename(
    SEV_unsafe_water = `Unsafe water source`,
    SEV_unsafe_sanitation = `Unsafe sanitation`,
    SEV_no_handwashing = `No access to handwashing facility`,
    SEV_iron_deficiency = `Iron deficiency`,
    SEV_VitA_deficiency = `Vitamin A deficiency`,
    SEV_Zinc_deficiency = `Zinc deficiency`,
    SEV_Growth = `Child growth failure`,
    SEV_low_temp = `Low temperature`,
    SEV_high_temp = `High temperature`
  ) #  SEV = summary exposure value

# Add a binary outcome: 1 = high burden, 0 = low burden
df_multi2 <- df_multi2 %>%
  mutate(
    high_burden = ifelse(
      enteric_deaths_pct > 10,
      1, 0
    )
  )

# Check how many regions are high vs. low burden
table(df_multi2$high_burden)
prop.table(table(df_multi2$high_burden)) # Class imbalance

# Consider scaling variables (Not necessary here)
# df_multi2 <- df_multi2 %>%
 #  mutate(across(starts_with("SEV_"), scale))

# Examine variables for a binary relationship (i.e is logistic regression appropriate)
library(ggpubr)
ggboxplot(df_multi2, x = "high_burden", y = "SEV_unsafe_water", color = "high_burden", add = "jitter")

# 2. Fit a simple logistic regression
# Use glm() with family = binomial to specify logistic regression

?glm

# Model: Unsafe water SEV predicting odds of high burden in that year
logit_model <- glm(
  high_burden ~ SEV_unsafe_water + SEV_iron_deficiency + SEV_unsafe_sanitation,
  data = df_multi2,
  family = binomial(link = "logit")
)

# Summarize the model
summary(logit_model)

# Tidy output: exponentiate coefficients to get odds ratios (OR)
tidy(logit_model, exponentiate = TRUE, conf.int = TRUE) # estimate is the beta but we've exponated to get our OR # run with exponentiate = FALSE


# Interpretation:
# - The odds ratio (OR) tells you how much the odds of being 'high burden'
#   increase for each 1-unit increase in SEV for unsafe water.
# - OR > 1 means higher SEV is associated with higher odds of high burden.


# In cases where predictors are non-linear or the relationship is complex, machine learning methods like decision trees, random forests, or gradient boosting may outperform logistic regression.

