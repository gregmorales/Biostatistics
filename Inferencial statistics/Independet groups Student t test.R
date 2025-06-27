# -----------------------------
# Inferencial statistics
# Student t for independent groups
# Dataset: NHANES (USA)
# Autor: Gregorio Morales
# -----------------------------

# Load packages
library(NHANES)
library(dplyr)
library(ggplot2)
library(rstatix)

# Load database
data("NHANES")

# Filter database with our variables of interest
NHANES_indepent_groups <- NHANES %>% 
  filter(Age >= 18, !is.na(BMI), !is.na(Gender))

# Data visualization
ggplot(NHANES_indepent_groups, aes(x =Gender, y = BMI, fill = Gender)) +
  geom_boxplot()+
  labs(title = "BMI distribution by sex", x = "Sex", y = "BMI")

# Student t test
NHANES_indepent_groups %>% 
  t_test(BMI~Gender, var.equal = FALSE) %>% 
  add_significance()

## There is no statistical significane between adult men and women BMI (p = 0.972)