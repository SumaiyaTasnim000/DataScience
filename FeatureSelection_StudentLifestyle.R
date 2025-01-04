library(tidyverse)
library(caret)
library(infotheo)
library(ggplot2)

dataset <- read.csv("E:\\10th Sem\\Data Science\\Final Lab\\student_lifestyle_dataset.csv", header = TRUE, sep = ",")
str(dataset)

dataset$Stress_Level <- as.factor(dataset$Stress_Level) #Pearson works only on numerical val, so convert into categorical

# Feature Selection Method 1: Pearson Correlation
# Pearson correlation requires numeric variables in input and output variable
correlation_results <- cor(dataset %>% select(-Stress_Level), use = "complete.obs") #find correlation among all the other attribute except goal attribute
correlation_with_stress <- sapply(dataset %>% select(-Stress_Level), function(x) { #find correlation between each coln and goal coln
  if (is.numeric(x)) cor(x, as.numeric(dataset$Stress_Level), use = "complete.obs") else NA #else NA if the column is not numeric
})
print("Pearson Correlation with Stress Level:")
print(correlation_with_stress)

# Create a data frame for correlation results
correlation_data <- data.frame(
  Feature = names(correlation_with_stress),
  Correlation = correlation_with_stress
)

# Plot a bar chart of the Pearson correlation results
ggplot(correlation_data, aes(x = reorder(Feature, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  ylim(-1, 1) +  # Set the y-axis range explicitly
  labs(
    title = "Pearson Correlation with Stress Level",
    x = "Features",
    y = "Correlation Coefficient"
  ) +
  theme_minimal()



# Feature Selection Method 2: ANOVA
# ANOVA evaluates relationships between a categorical target and numeric predictors
anova_results <- sapply(dataset %>% select(-Stress_Level), function(x) { #applies on all coln except goal coln
  if (is.numeric(x)) summary(aov(x ~ dataset$Stress_Level))[[1]][["Pr(>F)"]][1] else NA #checks the val of p 
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
  coord_flip() +  # Flip the axes to make it horizontal
  scale_color_manual(values = c("Significant" = "red", "Not Significant" = "gray")) + 
  labs(
    title = "ANOVA Results: P-Values for Features",
    x = "Features",
    y = "P-Value"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "blue") +  # Significance threshold line
  annotate("text", x = Inf, y = 0.05, label = "Significance Threshold (p = 0.05)", 
           color = "blue", hjust = 1.2, vjust = -1)



# Feature Selection Method 3: Mutual Information
# Mutual information measures non-linear relationships between variables
# Discreet the features, keeping column names intact
discretized_data <- data.frame(lapply(dataset[, -which(names(dataset) == "Stress_Level")], discretize)) #numerical features are converted into categorical.
colnames(discretized_data) <- colnames(dataset[, -which(names(dataset) == "Stress_Level")])  # Preserve column names(used for graph plotting)
dataset$Stress_Level <- as.factor(dataset$Stress_Level) # Convert 'Stress_Level' to a factor (for MI calculation)
mutual_info <- sapply(discretized_data, function(x) mutinformation(x, dataset$Stress_Level)) # Calculate Mutual Information for each feature with respect to goal attribute


# Convert the mutual_info results into a data frame
mutual_info_data <- data.frame(
  Feature = names(mutual_info),
  MI_Value = mutual_info
)

# Plot a bar chart of Mutual Information values
ggplot(mutual_info_data, aes(x = reorder(Feature, -MI_Value), y = MI_Value, fill = MI_Value)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +  # Flip axes for better readability
  scale_fill_gradient(low = "blue", high = "green") +  
  labs(
    title = "Mutual Information with Stress Level",
    x = "Features",
    y = "Mutual Information"
  ) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Add a line at MI = 0
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