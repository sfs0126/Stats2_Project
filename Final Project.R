library(tidyverse)
library(dplyr)
library(car)
library("nnet")


# Framingham Heart study dataset

##### From Kaggle 

### Dimitri Angelov and Steven Spielman


heart <- read_csv('framingham.csv')

heart <- heart %>% drop_na()


##### Data Exploration

cor(heart) > abs(0.8)

plot(heart)


##### Chi-Square

heart_table <- table(heart$education, heart$currentSmoker)

heart_table

chisq.test(heart_table)


##### Multiple Linear Regression

lm.obj <- lm(heartRate ~ .,
             data=heart)

summary(lm.obj)

step(lm.obj, trace = FALSE)

lm.reduced <- lm(heartRate ~ male + age + education + cigsPerDay + BPMeds + prevalentHyp + totChol + sysBP + diaBP + glucose,
                 data=heart)

summary(lm.reduced)

vif(lm.obj)
vif(lm.reduced)


##### Multiple Linear Regression Model Diagnostics

plot(lm.obj)


##### Multiple Linear Regression Outliers




##### Multiple Logistic Regression

glm.multiple <- glm(as.factor(TenYearCHD) ~ .,
                    data=heart,
                    family="binomial")

summary(glm.multiple)

step(glm.multiple)

glm.reduced <- glm(as.factor(TenYearCHD) ~ male + age + cigsPerDay + prevalentStroke + prevalentHyp + totChol + sysBP + glucose,
                   data=heart,
                   family="binomial")

summary(glm.reduced)

vif(glm.multiple) 
vif(glm.reduced) 
