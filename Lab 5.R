#Remove duplicate rows and outputs distinct rows
library(dplyr)
stats<-data.frame(player=c('A','B','C','D','C','A'),
                  runs=c(100,200,300,400,300,100),
                  wickets=c(17,20,NA,5,NA,17))
distinct(stats) 

#Remove duplicate column and outputs distinct column
library(dplyr)
stats<-data.frame(player=c('A','B','C','D','C','A'),
                  runs=c(100,200,300,400,300,100),
                  wickets=c(17,20,NA,5,NA,17))
distinct(stats,player, .keep_all=TRUE) 

# arrange method for desc to ascending sort
library(dplyr)
stats<-data.frame(player=c('A','B','C','D'),
                  runs=c(100,200,300,400),
                  wickets=c(17,20,NA,5))
arrange(stats,wickets) 
# asc to desc:
arrange(stats,desc(wickets))                    
arrange(stats,desc(runs))     

#Rename
rename(stats, runs_scored=runs)

#select method
select(stats,player,wickets)

#mutate and transmute 
mutate(stats,avg=runs/4)
transmute(stats,avg=runs/4)

#summarize
summarize(stats,sum(runs),mean(runs))

#Descriptive statistics
read_data<- read.csv("E:\\10th Sem\\Data Science\\Lab\\iris.csv", header= TRUE, sep=",")
print(read_data)
summary(read_data)

#value labels

read_data$Iris.setosa <- factor(read_data$Iris.setosa,
                            levels=c("Iris-setosa","Iris-versicolor", "Iris-virginica"),labels=c(1,2,3))
new_data <- read_data
print(new_data)

#min max normalization method
install.packages("caret")
library(caret)
read_data<- read.csv("E:\\10th Sem\\Data Science\\Lab\\iris.csv", header= TRUE, sep=",")
read_data <- subset(read_data, select=-c(Iris.setosa)) 
print(read_data)

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

normalisedMydata <- as.data.frame(lapply(read_data, minMax))
head(normalisedMydata)

#Standard Deviation
a<- read_data$X5.1
sd(a)
b<- read_data$X3.5
sd(b)
c<- read_data$X1.4
sd(c)
d<- read_data$X0.2
sd(d)

#(Alt)Standard Deviation on multiple numeric together
library(dplyr)

read_data %>%summarise_if(is.numeric,sd)