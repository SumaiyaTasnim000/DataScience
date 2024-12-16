install.packages("dplyr")
install.packages("visdat")  
library(dplyr)
library(visdat) 
#dataset_load
dataset <- read.csv("E:\\10th Sem\\Data Science\\Mid Project\\Midterm_Dataset_Section(B).csv", header = TRUE, sep = ",")
str(dataset)

#Summarizing no of col and rows
num_rows <- nrow(dataset)
num_cols <- ncol(dataset)
cat("The dataset has", num_rows, "rows and", num_cols, "columns.\n")

#visualizing the plot before any null handling
vis_miss(dataset)
summary(dataset)
str(dataset)

#Returns the sum of null values
colSums(is.na(dataset))
sum(is.na(dataset))

#Replacing null numerical values by mean,median and by mode for categorical
dataset$Age[is.na(dataset$Age)] <- mean(dataset$Age, na.rm = TRUE)
dataset$Study.Hours[is.na(dataset$Study.Hours)] <- median(dataset$Study.Hours, na.rm = TRUE)
gender_freq <- sort(table(dataset$Gender), decreasing = TRUE) 
gender_mode <- as.character(gender_freq[1]) 
dataset$Gender[is.na(dataset$Gender)] <- gender_mode

#Plotting the graph after null handling
vis_miss(dataset)
summary(dataset)
str(dataset)

#sum of duplicated data (same data matching across every columns)
sum(duplicated(dataset))
#removing duplicate columns
dataset <- dataset[!duplicated(dataset), ]
sum(duplicated(dataset))

#Dealing with invalid values
lapply(dataset[, sapply(dataset, is.character)], unique)
dataset[dataset == ""] <- NA
dataset$Have.you.ever.had.suicidal.thoughts<- recode(dataset$Have.you.ever.had.suicidal.thoughts,
                                                     "Yess" = "Yes", 
                                                     "Noo" = "No")
#Replacing categorical null values with the following:
dataset$Gender[is.na(dataset$Gender)] <- "Male" 
dataset$Sleep.Duration[is.na(dataset$Sleep.Duration)] <- "7-8 hours"  
dataset$Depression[is.na(dataset$Depression)] <- "No"
head(dataset) #shows first few rows

#Showing sum of all the categorical and not null values
sapply(dataset, class)
sapply(dataset, function(x) sum(!is.numeric(x) & !is.na(x)))

#Min to max range for numerical and not null values
sapply(dataset, function(x) sum(duplicated(x)))
sapply(dataset[, sapply(dataset, is.numeric)], function(x) range(x, na.rm = TRUE))


#Box plotting the range Before Outlier Handling
boxplot(dataset$Age, main = "Age (Before Outlier Handling)")
boxplot(dataset$Academic.Pressure, main = "Academic Pressure (Before Outlier Handling)")

#IQR 
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  return(list(outliers = x[x < lower | x > upper], 
              lower_bound = lower, 
              upper_bound = upper))
}

#Outlier info printing
numeric_columns <- sapply(dataset, is.numeric)
outlier_info <- lapply(dataset[, numeric_columns], detect_outliers)
outlier_info

#Box plotting after outlier handling
boxplot(dataset$Age, main = "Age (After Outlier Handling)")
boxplot(dataset$Academic.Pressure, main = "Academic Pressure (After Outlier Handling)")

#Dealing with outliers in age column
dataset$Age[dataset$Age %in% c(230, 226)] <- median(dataset$Age, na.rm = TRUE)
dataset$Academic.Pressure[dataset$Academic.Pressure %in% c(20, 15)] <- median(dataset$Academic.Pressure, na.rm = TRUE)

numeric_columns <- sapply(dataset, is.numeric)

#Binary columns: Label Encoding from categorical to numeric conversion
dataset$Gender <- ifelse(dataset$Gender == "Male", 1, 0)
dataset$Depression <- ifelse(dataset$Depression == "Yes", 1, 0)
dataset$Have.you.ever.had.suicidal.thoughts <- ifelse(dataset$Have.you.ever.had.suicidal.thoughts == "Yes", 1, 0)
dataset$Family.History.of.Mental.Illness <- ifelse(dataset$Family.History.of.Mental.Illness == "Yes", 1, 0)
unique(dataset$Sleep.Duration) 

dataset$Sleep.Duration <- as.numeric(factor(dataset$Sleep.Duration, 
                                            levels = c("Less than 5 hours","5-6 hours", "7-8 hours", "More than 8 hours"), 
                                            labels = c(1, 2, 3,4)))
unique(dataset$Dietary.Habits)  
dataset$Dietary.Habits <- as.numeric(factor(dataset$Dietary.Habits, 
                                            levels = c("Unhealthy", "Moderate", "Healthy"), 
                                            labels = c(1, 2, 3)))
# View the updated dataset
head(dataset)

#Min-Max scaling normalization
dataset$Age <- (dataset$Age - min(dataset$Age)) / (max(dataset$Age) - min(dataset$Age))
head(dataset)

#Encoded categories are shown as 1,2,3 labels and the sum under each label
table(dataset$Gender)
table(dataset$Sleep.Duration)
table(dataset$Dietary.Habits)
table(dataset$Have.you.ever.had.suicidal.thoughts)
table(dataset$Family.History.of.Mental.Illness)
table(dataset$Depression)

#No of yes and no observations
yes_obs <- which(dataset$Depression == 1)
no_obs <- which(dataset$Depression == 0)

#No of sum of yes and no observations
yes_count <- length(yes_obs)
no_count <- length(no_obs)
print(yes_count) # 83 yes obs
print(no_count) # 117 no obs, no has larger count here 

#Oversampling the Minority Class
balanced_yes_sample <- sample(yes_obs, no_count, replace = TRUE)

#Create the Oversampled Dataset:
oversampled_dataset <- dataset[c(balanced_yes_sample, no_obs), ]

#Verifying the balance (yes obs = 117 and no obs = 117 )
oversampled_dataset %>% count(Depression)

#Undersampling
yes_obs <- which(dataset$Depression == 1)
no_obs <- which(dataset$Depression == 0)
yes_count <- length(yes_obs)
no_count <- length(no_obs)
balanced_no_sample <- sample(no_obs, yes_count, replace = FALSE) #undersampling the majority class
undersampled_dataset <- dataset[c(yes_obs, balanced_no_sample), ]
undersampled_dataset %>% count(Depression)

write.csv(oversampled_dataset, "Assignment1_Processed.csv",)
print ('CSV file written Successfully :)')


