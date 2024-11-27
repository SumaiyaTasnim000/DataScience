#Detecting missing values, True= missing value
read_data<- read.csv("E:\\10th Sem\\Data Science\\Lab\\iris1.csv", header= TRUE, sep=",")
print(read_data)
is.na(read_data)

# detecting number of null values
colSums(is.na(read_data))

#Detecting in which instance there is null value
which(is.na(read_data$X1.4))

# Remove null values
remove <- na.omit((read_data))
print(remove)

