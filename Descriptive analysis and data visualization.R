# -----------------------------
# Basic descriptive analysis with R
# Dataset: NHANES (USA)
# Autor: Gregorio Morales
# -----------------------------


#1. Load packages
library(tidyverse) #For data manipulation
library(NHANES) #Health related dataset
library(janitor) #For clean tables
library(summarytools) #For sumary tables
library(ggplot2) #For data visualization

#2. Load data from package "NHANES".
data("NHANES")
df <- NHANES

#3. Check structure
glimpse(df)
summary(df)

#4. Select of variables of interest 
variables <- c("Age", "Gender", "BMI", "Diabetes", "TotChol", "PhysActive", "SmokeNow")
df <- df %>% 
  select(all_of(variables)) %>% 
  drop_na()
df

#5. Description of continous variables
df %>% 
  select(where(is.numeric)) %>% 
  descr(stats = c("mean", "sd", "min", "max", "iqr"), transpose = TRUE)

#6. Frecuency of categorical variables
df %>% 
  select(where(is.factor)) %>%
  map(~ freq(.x))

#7. Data visualization for quantitative variables

##7.1 Histogram
ggplot(df, aes(x = BMI)) +
  geom_histogram(binwidth = 2, fill = "blue", color = "white") +
  labs(title = "BMI distribution", x = "BMI", y = "Frecuency")

##7.2 Boxplot
ggplot(df, aes(x = Gender, y = BMI)) +
  geom_boxplot(fill = "grey") + 
  labs(title = "BMI according to gender")

##7.3 Scatter plot
ggplot(df, aes(x = TotChol, y = BMI)) +
  geom_point()+
  labs(title = "BMI and Total cholesterol relationship", x = "Total Cholesterol", y = "BMI")

##7.4 Density plot
ggplot(df, aes(x = BMI, fill = Gender))+
  geom_density(alpha = 0.4)+
  labs(title = "Density curve of BMI according to gender")

#7.5 Violin plot
ggplot(df, aes(x = Diabetes, y = Age)) + 
  geom_violin()+
  labs(title = "Age of participants according to diabetes diagnosis")

#8. Data visualization of qualitative variables

##8.1 Bar plot
ggplot(df, aes(x = SmokeNow, fill = SmokeNow)) +
  geom_bar() +
  labs(title = "Frecuencia de pacientes que fuman ahora", x = "Fuman ahora", y = "Frecuencia") +
  scale_fill_manual(values = c("tomato", "skyblue"))

##8.2 Stacked bar plot
ggplot(df, aes (x = Gender, fill = Diabetes))+
  geom_bar(position = "fill")+
  labs(title = "Proportion of diabetes according to biological sex", x = "Gender", y = "Proportion")

##8.3 Dodged bar plot
ggplot(df, aes(x = Gender, fill = PhysActive)) + 
  geom_bar(position = "dodge")+
  labs(title = "Distribution of physical activity for sex", x = "Gender", y = "Frecuency")

##8.4 Pie chart
df %>% 
  count(SmokeNow) %>%
  ggplot(aes(x = "", y = n, fill = SmokeNow)) + 
  geom_col() + 
  coord_polar(theta = "y")+
  labs(title = "Proportion of persons that smokes now", )

system('git config --global credential.helper ""')
