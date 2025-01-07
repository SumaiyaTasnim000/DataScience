library(tidyverse)
library(caret)
library(infotheo)
library(ggplot2)
library(gridExtra)  


dataset <- read.csv("E:\\10th Sem\\Data Science\\Final Lab\\LabTask1\\student_lifestyle_dataset.csv", header = TRUE, sep = ",")
str(dataset)

dataset$Stress_Level <- as.factor(dataset$Stress_Level) #goal attribute column is considered as a factor & converted into numerical codes.
# Feature Selection Method 1: Pearson Correlation
# Pearson correlation requires numeric variables in both target and predictors
correlation_results <- cor(dataset %>% select(-Stress_Level), use = "complete.obs") #finds correlation among all features except goal feature
correlation_with_stress <- sapply(dataset %>% select(-Stress_Level), function(x) { #calculates the Pearson correlation between each feature (excluding Stress_Level) and the Stress_Level column.
  if (is.numeric(x)) cor(x, as.numeric(dataset$Stress_Level), use = "complete.obs") else NA #checks if the feature is numeric, else NA
})
print("Pearson Correlation with Stress Level:")
print(correlation_with_stress)

# Create a data frame for correlation results
correlation_data <- data.frame(
  Feature = names(correlation_with_stress),
  Correlation = correlation_with_stress
)
# Plot a line graph of the Pearson correlation results with pos and neg correlations
positive_correlation_data <- correlation_data %>% filter(Correlation >0)
negative_correlation_data <- correlation_data %>% filter(Correlation < 0)

positive_plot <- ggplot(positive_correlation_data, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Smoothed fitted line
  geom_point(color = "red", size = 3) +  # Data points
  ylim(0, 1) +  
  labs(
    title = "Positive Pearson Correlations with Stress Level",
    x = "Features",
    y = "Correlation Coefficient"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Negative Correlation Plot
negative_plot <- ggplot(negative_correlation_data, aes(x = reorder(Feature, Correlation), y = Correlation)) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # Smoothed fitted line
  geom_point(color = "red", size = 3) +  # Data points
  ylim(-1, 0) +  
  labs(
    title = "Negative Pearson Correlations with Stress Level",
    x = "Features",
    y = "Correlation Coefficient"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   

gridExtra::grid.arrange(positive_plot, negative_plot, ncol = 2)



# Feature Selection Method 2: ANOVA
# ANOVA evaluates relationships between numeric predictors and a categorical target 
#The p-value tells us whether the feature is significantly different across stress levels.
anova_results <- sapply(dataset %>% select(-Stress_Level), function(x) { #applied function to all coln except goal coln
  if (is.numeric(x)) summary(aov(x ~ dataset$Stress_Level))[[1]][["Pr(>F)"]][1] else NA  #[[1]][["Pr(>F)"]][1] extracts the p-value for the feature x
})
print("ANOVA p-values:")
print(anova_results)

# Create a data frame for ANOVA results
anova_data <- data.frame(
  Feature = names(anova_results),
  P_Value = as.numeric(anova_results)
)

# Add a new column for significance (TRUE if p-value < 0.05, FALSE otherwise)
anova_data$Significance <- ifelse(anova_data$P_Value < 0.05, "Significant", "Not Significant")

# Plot a dot plot for the ANOVA results
ggplot(anova_data, aes(x = reorder(Feature, P_Value), y = P_Value, color = Significance)) +
  geom_point(size = 4) +  
  coord_flip() +  
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "gray")) + 
  labs(
    title = "ANOVA Results: P-Values for Features",
    x = "Features",
    y = "P-Value"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") + 
  annotate("text", x = Inf, y = 0.05, label = "Significance Threshold (p = 0.05)", 
           color = "blue", hjust = 1.2, vjust = -1)



# Feature Selection Method 3: Mutual Information
# Mutual information measures non-linear relationships between variables
# Discretize the features, keeping column names intact
discretized_data <- data.frame(lapply(dataset[, -which(names(dataset) == "Stress_Level")], discretize)) #converting from numerical to categorical for predictor classes
colnames(discretized_data) <- colnames(dataset[, -which(names(dataset) == "Stress_Level")]) 
dataset$Stress_Level <- as.factor(dataset$Stress_Level) # Convert 'Stress_Level' to a factor (for MI calculation)
mutual_info <- sapply(discretized_data, function(x) mutinformation(x, dataset$Stress_Level)) # Calculate Mutual Information for each feature with respect to 'Stress_Level'


# Convert the mutual_info results into a data frame
mutual_info_data <- data.frame(
  Feature = names(mutual_info),
  MI_Value = mutual_info
)


# Plot a bar chart of Mutual Information values
ggplot(mutual_info_data, aes(x = reorder(Feature, -MI_Value), y = MI_Value, fill = MI_Value)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() + 
  scale_fill_gradient(low = "blue", high = "green") +  
  labs(
    title = "Mutual Information with Stress Level",
    x = "Features",
    y = "Mutual Information"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") + 
  annotate("text", x = Inf, y = 0, label = "MI = 0 (No Information)", color = "red", hjust = 1.2, vjust = -1)

# Summarize results
results <- data.frame(
  Feature = names(dataset %>% select(-Stress_Level)),
  Pearson_Correlation = correlation_with_stress,
  ANOVA_p_value = anova_results,
  Mutual_Information = mutual_info
)

# Print the summarized results
print("Feature Selection Results:")
print(results)