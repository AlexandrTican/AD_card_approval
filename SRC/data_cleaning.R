install.packages("dplyr")
install.packages("tidyr")
install.packages("glmnet")
install.packages("ROCR")
install.packages("tidyverse")
install.packages("scatterplot3d")

library(dplyr)
library(tidyr)
library(glmnet)
library(ROCR)
library(tidyverse)
library(ggplot2)
library(scatterplot3d)

# Load datasets
application_data <- read.csv("C:/utm year 3/AD/application_record.csv")
credit_data <- read.csv("C:/utm year 3/AD/credit_record.csv")

# Merge datasets by 'ID'
merged_data <- merge(application_data, credit_data, by = "ID")

# Limit the dataset to 10,000 entries
limited_data <- head(merged_data, 10000)

# later save it to CSV after creating binary column based on debt status

write.csv(limited_data, 'limited_data.csv', row.names = FALSE) 

# Load the limited_data
limited_data <- read.csv("C:/utm year 3/AD/limited_data.csv")  # Specify the correct file path

# Drop duplicates
limited_data <- limited_data[!duplicated(limited_data[,2:ncol(limited_data)]), ]

# Fill missing values
limited_data$OCCUPATION_TYPE[is.na(limited_data$OCCUPATION_TYPE)] <- 'Other'

# Drop constant feature
limited_data <- subset(limited_data, select = -FLAG_MOBIL)


# Extract account length (Adjust column names accordingly)
start_df <- limited_data %>%
  group_by(ID) %>%
  summarize(ACCOUNT_LENGTH = min(MONTHS_BALANCE)) %>%
  mutate(ACCOUNT_LENGTH = -ACCOUNT_LENGTH)

# Merge dataframes
limited_data <- merge(limited_data, start_df, by = 'ID', all.x = TRUE)

# Create age feature (Adjust column names accordingly)
limited_data$AGE_YEARS <- -limited_data$DAYS_BIRTH / 365.2425
limited_data <- subset(limited_data, select = -DAYS_BIRTH)

# Create unemployed indicator (Adjust column names accordingly)
limited_data$UNEMPLOYED <- 0
limited_data$UNEMPLOYED[limited_data$DAYS_EMPLOYED < 0] <- 1

# Create years employed feature (Adjust column names accordingly)
limited_data$YEARS_EMPLOYED <- -limited_data$DAYS_EMPLOYED / 365.2425
limited_data$YEARS_EMPLOYED[limited_data$YEARS_EMPLOYED < 0] <- 0
limited_data <- subset(limited_data, select = -DAYS_EMPLOYED)

# Encode binary features
limited_data$CODE_GENDER <- ifelse(limited_data$CODE_GENDER == 'F', 0, 1)
limited_data$FLAG_OWN_CAR <- ifelse(limited_data$FLAG_OWN_CAR == 'Y', 1, 0)
limited_data$FLAG_OWN_REALTY <- ifelse(limited_data$FLAG_OWN_REALTY == 'Y', 1, 0)

# Rename columns (Adjust column names accordingly)
names(limited_data) <- c('ID', 'Gender', 'Own_car', 'Own_property', 'Work_phone',
                         'Phone', 'Email', 'Unemployed', 'Num_children', 'Num_family', 
                         'Account_length', 'Total_income', 'Age', 'Years_employed',  
                         'Income_type', 'Education_type', 'Family_status',
                         'Housing_type', 'Occupation_type', 'Target')

# Data types
limited_data$Num_family <- as.integer(limited_data$Num_family)

# Re-order columns
limited_data <- limited_data[, c('ID', 'Gender', 'Own_car', 'Own_property', 'Work_phone',
                                 'Phone', 'Email', 'Unemployed', 'Num_children', 'Num_family', 
                                 'Account_length', 'Total_income', 'Age', 'Years_employed',  
                                 'Income_type', 'Education_type', 'Family_status',
                                 'Housing_type', 'Occupation_type', 'Target')]

limited_data[limited_data == ""] <- NA


# Save to CSV
write.csv(limited_data, 'clean_data.csv', row.names = FALSE)

clean_data <-
# Stacked bar plot of Family_status by Approval Status
status_table <- table(credit_data$Family_status, credit_data$Status)
barplot(status_table, 
        main="Stacked Bar Plot of Family_status by Status",
        xlab="Family_status", ylab="Frequency",
        col=c("skyblue", "lightgreen"), legend=c("Not Approved", "Approved"))

summary(c_data)