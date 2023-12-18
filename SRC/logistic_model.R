# Install and load necessary libraries
install.packages("stringi")
library(stringi)
install.packages(c("modeldata", "tidyverse", "caret", "ROCR", "vip"))
library(modeldata)
library(tidyverse)
library(caret)
library(ROCR)
library(vip)
library(dplyr)
library(rsample)

# Load your dataset (replace 'dataset.csv' with your actual file name)
data <- read.csv("C:/utm year 3/AD/clean_data.csv")


# Explore the dataset
dim(data)
glimpse(data)
head(data$Target)
df <- data %>% mutate_if(is.ordered, factor, ordered = FALSE)

# Ensure Target is a factor with two levels
df$Target <- factor(df$Target, levels = c("0", "1"))

# Create training (70%) and test (30%) sets
churn_split <- initial_split(df, prop = 0.7, strata = 'Target')
churn_train <- training(churn_split)
churn_test <- testing(churn_split)

# Single predictor models
model1 <- glm(Target ~ Total_income, family = 'binomial', data = churn_train)
model2 <- glm(Target ~ Account_length, family = 'binomial', data = churn_train)

# Display coefficients for model1 and model2
summary(model1)
summary(model2)

# Multiple logistic regression
model3 <- glm(
  Target ~ Gender + Own_car + Own_property + Work_phone + Phone + Email + 
    Unemployed + Num_children + Num_family + Account_length + Total_income + 
    Age + Years_employed + Income_type + Education_type + Family_status + 
    Housing_type + Occupation_type,
  family = 'binomial',
  data = churn_train
)

# Display coefficients for model3
summary(model3)

# Model assessment using cross-validation
set.seed(123)
cv_model1 <- train(
  Target ~ Total_income,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model2 <- train(
  Target ~ Account_length, 
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

set.seed(123) 
cv_model3 <- train(
  Target ~ Gender + Own_car + Own_property + Work_phone + Phone + Email + 
    Unemployed + Num_children + Num_family + Account_length + Total_income + 
    Age + Years_employed + Income_type + Education_type + Family_status + 
    Housing_type + Occupation_type,
  data = churn_train,
  method = 'glm',
  family = 'binomial',
  trControl = trainControl(method = 'cv', number = 10)
)

# Display accuracy from cross-validation
summary( 
  resamples(
    list(
      model1 = cv_model1, model2 = cv_model2, model3 = cv_model3
    ))
)$statistics$Accuracy

# Predict class, create confusion matrix
pred_class <- predict(cv_model3, churn_train)
confusionMatrix(
  data = relevel(as.factor(pred_class), ref = '1'),
  reference = relevel(churn_train$Target, ref = '1')
)

# Compute predicted probabilities
m1_prob <- predict(cv_model1, churn_train, type = 'prob')$`1`
m3_prob <- predict(cv_model3, churn_train, type = 'prob')$`1`

# Compute AUC metrics for cv_model1 and cv_model3
perf1 <- prediction(m1_prob, churn_train$Target) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')
perf2 <- prediction(m3_prob, churn_train$Target) %>% 
  performance(measure = 'tpr', x.measure = 'fpr')

# Plot ROC curves for cv_model1 and cv_model3
plot(perf1, col = 'black', lty = 2)
plot(perf2, add = TRUE, col = 'blue')
legend(0.8, 0.2, legend = c('cv_model1', 'cv_model3'),
       col = c('black', 'blue'), lty = 2:1, cex = 0.6)

# Feature interpretation
vip(cv_model3, num_features = 20)

