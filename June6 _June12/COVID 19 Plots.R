library(tidyverse)
library(ggplot2)


dataset = read.csv("June6 _June12/COVID19_line_list_data.csv")
View(dataset)



# Filtering out dataset for no. of males and females countrywise
male<- dataset%>% group_by(country,gender) %>% filter(gender == "male") %>% summarize(count=n()) 
female<- dataset%>% group_by(country,gender) %>% filter(gender == "female") %>% summarize(count=n())   


# Plot of No. Of Infected Males per country 
Plot1 <- ggplot(male) + geom_col(mapping = aes(x = country, y = count),fill ="blue", alpha = 0.5) 
Plot1 + labs(title = "No. Of Infected Males per country",
             x = "Country", y ="No. of Infected Males") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


# Plot of No. Of Infected Females per country
Plot2 <- ggplot(female) + geom_col(mapping = aes(x = country, y = count),fill ="red", alpha = 0.5) 
Plot2 + labs(title = "No. Of Infected Females per country",
             x = "Country", y ="No. of Infected Females")+
  theme(axis.text.x=element_text(angle = -90, hjust = 0))



# Combined plot of No. Of Infected Males and Females per country
ggplot() + geom_col(data = male,mapping = aes(x = country, y = count),fill ="blue") +
  geom_col(data = female,mapping = aes(x = country, y = count),fill ="red") +
  labs(title = "No. of Infected Males and Females in the country",x = "Country",y ="Number of people") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(panel.grid.major = element_blank(),
         axis.line = element_line(colour = "black"))




# As we can see the columns in the previous plot gets printed one over the other, so we are going to plot our data in such a way that
# the bars dont get printed one over the other but rather get stacked for both male and female count
# New dataset containing Male and female counts in long format
male_female_combined_dataset <- bind_rows(male,female)

#Attempt to plot the column graph for each gender individually
ggplot(male_female_combined_dataset,aes(x=country, y= count, fill = gender) ) +
  geom_col(position ="dodge")+
  labs(title = "No. of Infected Males and Females in the country",x = "Country",y ="Number of people") +
  theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
  theme(panel.grid.major = element_blank(),
        axis.line = element_line(colour = "black"))+
  theme_bw()





# Filtering our dataset based on Country, Reporting Date and plotting the no. of cases

no_of_cases_per_day <- dataset %>% group_by(reporting.date) %>% filter(country == "China") %>% summarize(count =n())

ggplot(no_of_cases_per_day) +
  geom_col(mapping = aes(x= reporting.date, y= count),fill = "cyan") + 
  labs(title = "No. Of Cases per day",
             x = "Reporting Date", y ="No. of cases") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"))




#Scatter Plot for No. of deaths vs Age 
no_of_deaths_vs_age<- dataset %>% filter(death == "1") %>% group_by(age) %>% summarize(count = n())


ggplot(no_of_deaths_vs_age) + geom_point(mapping =aes(x=age, y= count), shape = "star") +
  xlim(0,90) + 
  scale_x_binned() +
  labs(title = "No. of deaths vs Age",
       x = "Age", y ="No. of cases") +
  theme_bw()+
  theme(axis.line = element_line(colour = "black"))







