# List
g <- "My 1st list"
h <- c(25,26,18,39)
j <- matrix(1:10,nrow=5)
k <- c("one", "two", "Three")
mylist <- list(title=g,ages= h,j,k)
print(mylist)

#User Input with readline()
var1 = readline(prompt = "Enter any value:" )
print(var1)

var2 = readline(prompt = "Enter any number:" )
var2 = as.integer(var2)
print(var2)

#User Input with scan()
x=scan()
print(x)

#Entering data from the keyboard

mydata1<-data.frame(age=numeric(0),gender=character(0),weight=numeric(0))
mydata1 <-edit(mydata1)
print(mydata1)

#Writing in csv file
Country <- c("China", "India", "United States", "Indonesia", "Pakistan") 

Population_1_july_2018 <- c("1,427,647,786", "1,352,642,280", 
                            "327,096,265", "267,670,543", "212,228,286") 

Population_1_july_2019 <- c("1,433,783,686", "1,366,417,754", 
                            "329,064,917", "270,625,568", "216,565,318") 

change_in_percents <- c("+0.43%", "+1.02%", "+0.60%", "+1.10%", "+2.04%") 


data <- data.frame(Country, Population_1_july_2018, Population_1_july_2019, change_in_percents) 
print(data) 

write.csv(data,"E:\\10th Sem\\Data Science\\Lab\\NEWCSV\\population.csv") 
print ('CSV file written Successfully :)')

#Read csv file
read_data <- read.csv("E:\\10th Sem\\Data Science\\Lab\\iris.csv", header= TRUE, sep=",")
print(read_data)

# install packages to manipulate data
install.packages("dplyr")
library(dplyr)

#using filter function
library(dplyr)
stats<-data.frame(player=c('A','B','C','D'),
                  runs=c(100,200,300,400),
                  wickets=c(17,20,NA,5))
filter(stats,runs>100)