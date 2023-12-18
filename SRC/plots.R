  # Load necessary libraries
  library(ggplot2)
  library(dplyr)
  
  # Load your c_dataset
  c_data <- read.csv("C:/utm year 3/AD/clean_data.csv")
  # Define suitable colors for the Target variable
  target_colors <- c("0" = "#4CAF50", "1" = "#FF2222")
  
  

  # Box plot: Total Income by Target
  ggplot(c_data, aes(x = factor(Target), y = Total_income, fill = factor(Target))) +
    geom_boxplot() +
    labs(title = "Box Plot: Total Income by Target", x = "Target", y = "Total Income", fill = "Target") +
    scale_fill_manual(values = target_colors)
  
  
  # Bar plot: Family Status distribution by Target
  ggplot(c_data, aes(x = factor(Family_status), fill = factor(Target))) +
    geom_bar(position = "dodge") +
    labs(title = "Family Status Distribution by Target", x = "Family Status", y = "Count", fill = "Target") +
    scale_fill_manual(values = target_colors)
  
  # Scatter plot: Years Employed vs. Target
  ggplot(data, aes(x = Years_employed, y = factor(Target), color = factor(Target))) +
    geom_point() +
    labs(title = "Scatter Plot: Years Employed vs. Target", x = "Years Employed", y = "Target", color = "Target") +
    scale_color_manual(values = target_colors)
  
  # Scatter plot: Total Income vs. Target
  ggplot(data, aes(x = Total_income, y = factor(Target), color = factor(Target))) +
    geom_point() +
    labs(title = "Scatter Plot: Total Income vs. Target", x = "Total Income", y = "Target", color = "Target") +
    scale_color_manual(values = target_colors)
  
  # Bar plot: Unemployed distribution by Target
  ggplot(data, aes(x = factor(Unemployed), fill = factor(Target))) +
    geom_bar(position = "dodge") +
    labs(title = "Unemployed Distribution by Target", x = "Unemployed", y = "Count", fill = "Target") +
    scale_fill_manual(values = target_colors)
  
  # Bar plot: Own Property distribution by Target
  ggplot(data, aes(x = factor(Own_property), fill = factor(Target))) +
    geom_bar(position = "dodge") +
    labs(title = "Own Property Distribution by Target", x = "Own Property", y = "Count", fill = "Target") +
    scale_fill_manual(values = target_colors)
 
  
  
  
  # Define suitable colors for the Target variable
  target_colors <- c("0" = "#4CAF50", "1" = "#FF2222")
  


  
  # Histogram: Distribution of Age
  ggplot(c_data, aes(x = Age, fill = factor(Target))) +
    geom_histogram(binwidth = 5, position = "dodge", alpha = 0.7) +
    labs(title = "Histogram: Distribution of Age",
         x = "Age",
         y = "Count",
         fill = "Target") +
    scale_fill_manual(values = target_colors)

  # Box Plot: Years_employed by Target
  ggplot(c_data, aes(x = factor(Target), y = Years_employed, fill = factor(Target))) +
    geom_boxplot() +
    labs(title = "Box Plot: Years_employed by Target",
         x = "Target",
         y = "Years Employed",
         fill = "Target") +
    scale_fill_manual(values = target_colors)
  
  # Bar Plot: Distribution of Occupation_type
  ggplot(c_data, aes(x = Occupation_type, fill = factor(Target))) +
    geom_bar(position = "dodge") +
    labs(title = "Bar Plot: Distribution of Occupation_type",
         x = "Occupation Type",
         y = "Count",
         fill = "Target") +
    scale_fill_manual(values = target_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Box Plot: Account_length by Target
  ggplot(c_data, aes(x = factor(Target), y = Account_length, fill = factor(Target))) +
    geom_boxplot() +
    labs(title = "Box Plot: Account_length by Target",
         x = "Target",
         y = "Account Length",
         fill = "Target") +
    scale_fill_manual(values = target_colors)
  
  # Bar Plot: Distribution of Employment Status (Unemployed) by Target
  ggplot(c_data, aes(x = factor(Unemployed), fill = factor(Target))) +
    geom_bar(position = "dodge") +
    labs(title = "Bar Plot: Distribution of Employment Status by Target",
         x = "Employment Status (Unemployed)",
         y = "Count",
         fill = "Target") +
    scale_fill_manual(values = target_colors)
  
summary(c_data)
