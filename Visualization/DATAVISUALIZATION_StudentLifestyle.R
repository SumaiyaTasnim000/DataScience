library(ggplot2)
library(dplyr)
library(e1071)  # For skewness calculation
library(tidyr) #for long format
library(GGally) #for scatterplot matrix


dataset <- read.csv("E:\\10th Sem\\Data Science\\Final Lab\\LabTask1\\student_lifestyle_dataset.csv", header = TRUE, sep = ",")
str(dataset)


##Univariate Analysis Tools
#1.Histogram (Counting down the most repetitive value amongst a feature)
ggplot(dataset, aes(x = Study_Hours_Per_Day)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Study Hours Per Day", x = "Study Hours Per Day", y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#2. Line Histogram (Histogram with a Density Curve)
ggplot(dataset, aes(x = Study_Hours_Per_Day)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightblue", color = "black", alpha = 0.5) +
  geom_density(color = "red", size = 1) +  # Adds density curve
  labs(title = "Line Histogram", x = "Study Hours Per Day", y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))  

#3. Skewness Analysis (Detecting Positive/Negative Skewness in Data)
#Skewness: whether the data is symmetrical (50%-50%), right-skewed(more than 50% at right side), or left-skewed (more than 50% at left side).

# Compute skewness, mean, median, and mode for Study_Hours_Per_Day
# Calculate statistics
skew_value <- skewness(dataset$Study_Hours_Per_Day, na.rm = TRUE)  
mean_value <- mean(dataset$Study_Hours_Per_Day, na.rm = TRUE)
median_value <- median(dataset$Study_Hours_Per_Day, na.rm = TRUE)
mode_value <- as.numeric(names(sort(table(dataset$Study_Hours_Per_Day), decreasing = TRUE)[1]))

# Create density plot
ggplot(dataset, aes(x = Study_Hours_Per_Day)) +
  geom_density(fill = "lightblue", color = "blue", size = 1.2, adjust = 1.5) +  
  geom_vline(xintercept = mean_value, color = "red", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = median_value, color = "black", linetype = "dashed", size = 1) +  
  geom_vline(xintercept = mode_value, color = "orange", linetype = "dashed", size = 1) +  
  annotate("text", x = mean_value, y = 0.02, label = "Mean", color = "red", vjust = -1.5, size = 5) +
  annotate("text", x = median_value, y = 0.02, label = "Median", color = "black", vjust = -1.5, size = 5) +
  annotate("text", x = mode_value, y = 0.02, label = "Mode", color = "orange", vjust = -1.5, size = 5) +
  labs(title = paste("Density Plot with Skewness =", round(skew_value, 2)), 
       x = "Study Hours Per Day", y = "Density") +
  theme_minimal()


##Multivariate Analysis Tools
#1.Violin Plot (Feature Distributions Across Stress Levels)
#Wider sections → More students study that many hours & Narrower sections → Fewer students study that many hours.
# Convert dataset into long format for faceted violin plots
long_data <- dataset %>% 
  pivot_longer(cols = -c(Student_ID, Stress_Level),  # Exclude ID and target variable
               names_to = "Feature", 
               values_to = "Value")
# Violin plot with box plot in the middle
ggplot(long_data, aes(x = Stress_Level, y = Value, fill = Stress_Level)) +
  geom_violin(alpha = 0.7) +  # Violin plot with transparency
  geom_boxplot(width = 0.2, outlier.shape = NA, color = "black") +  # Add box plot inside
  facet_wrap(~ Feature, scales = "free_y") +  # Separate plots for each feature
  theme_minimal() +
  labs(title = "Violin Plot of Features Across Stress Levels") +
  theme(legend.position = "none")  # Remove redundant legend


# 2. Scatterplot matrix
# Select only numerical features (excluding Student_ID and Stress_Level)
# Select numerical features (excluding Student_ID)
numerical_data <- dataset %>% select(-Student_ID)
# Convert Stress_Level to factor for color grouping
numerical_data$Stress_Level <- as.factor(dataset$Stress_Level)
# Generate scatterplot matrix with a visible legend
ggpairs(numerical_data, 
        aes(color = Stress_Level, alpha = 0.6),  # Ensure color is mapped correctly
        lower = list(continuous = wrap("points", size = 2)),  # Scatterplots only
        upper = list(continuous = "blank"),  # Remove upper half
        diag = list(continuous = "blank")  # Remove diagonal density plots
)  

#3.Line Graphs
# Line graph for each feature considering average 
ggplot(long_data, aes(x = Stress_Level, y = Value, group = Feature, color = Feature)) +
  geom_line(stat = "summary", fun = mean, size = 1) +  
  geom_point(stat = "summary", fun = mean, size = 3) +
  facet_wrap(~ Feature, scales = "free_y") + 
  theme_minimal() +
  labs(title = "Line Graphs Across Stress Levels", x = "Stress Level", y = "Average Value")

