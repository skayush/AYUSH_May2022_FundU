rm(list=ls())      #To remove all variables stored previously
library(Hmisc)

#reading and viewing the data
data <- read.csv("C:/Users/DELL/Desktop/COVID19_line_list_data.csv")
describe(data)

# cleaning of data
data$death_dummy = as.integer(data$death !=0)
#death rate
sum(data$death_dummy)/ nrow(data)

#Checking average age of people who died from the virus
dead = subset(data, death_dummy ==1)
alive =subset(data, death_dummy ==0)
mean(dead$age, na.rm =TRUE)
mean(alive$age, na.rm =TRUE)

#To check with the help of statistics if our analysis is true
t.test(alive$age, dead$age, alternative="two.sided", conf.level = 0.95)
#If p-value is less than 0.05 then or hypothesis is wrong
#In this case p-value almost =0, hence our hypothesis is correct.

#Checking the genderof people who died from the virus
male = subset(data, gender == "male")
female =subset(data, gender == "female")
mean(male$death_dummy, na.rm =TRUE)
mean(female$death_dummy, na.rm =TRUE)

#To check with the help of statistics if our analysis is true
t.test(male$death_dummy, female$death_dummy, alternative="two.sided", conf.level = 0.95)
#If p-value is less than 0.05 then or hypothesis is wrong
#In this case p-value almost = 0, hence our hypothesis is correct.