#Gaurab Das

# A VISUAL HISTORY OF NOBEL PRIZE WINNERS (1901-2016)

# Import libraries
library('tidyverse')
library('lubridate')
library('dplyr')

#1. Load dataset
nobel = read_csv("D:/Data Science/R/nobel-laureates/archive.csv")
nobel= as.data.frame(nobel)

# View the first few rows
head(nobel)

# Column Names 
colnames(nobel)

#2.Who gets Nobel Prize

# Count the number of shared and not shared Nobel Prizes handed
x = nobel %>% count(`Prize Share` == '1/1') %>% arrange(n)
colnames(x) = c("Shared", "Count") 
x



# Count the number of prizes won by male and female recipients.
nobel %>% group_by(`Sex`) %>% count()   


# Count the number of prizes won by different nationalities.
nobel %>% group_by(`Birth Country`) %>% count() %>% arrange(desc(n))

#3.USA Dominance

# Calculate the proportion of USA born winners per decade
prop_usa_winners <- nobel %>% mutate(usa_born_winner =
                    ifelse(`Birth Country` == "United States of America",TRUE,FALSE)) %>% 
                    mutate(decade  = `Year` - `Year` %% 10 )


prop <- prop_usa_winners %>% group_by(`decade`) %>% summarize(proportion = mean(`usa_born_winner`, na.rm = TRUE)) 
prop

#verify Results
b=filter(prop_usa_winners, decade == 1900 & usa_born_winner == 'TRUE')
c=filter(prop_usa_winners, decade == 1900) 

length(b$Year)/(length(c$Year)-length(b$Year))
 
prop[1,] # Verified



#4. USA dominance, visualized

#plot decade vs proportion, fit a line, highlight points, scale proportions to percentages
#check limits and cut out extra area of the graph
ggplot(prop, aes(decade,proportion)) + geom_line() + geom_point() +
                scale_y_continuous( limits = c(0.0,0.5),
                labels = scales::percent, expand = c(0,0)) 



#5.Gender of a typical Nobel Prize winner? 

#Calculating proportions of a female Laureates per decade
prop_female_winners <- nobel %>% mutate(female_winner = ifelse(Sex == "Female", TRUE, FALSE)) %>%
                      mutate(decade = Year - Year %% 10) %>% group_by(decade, Category) %>% 
                      summarize(proportion = mean (female_winner, na.rm =TRUE))

prop_female_winners

#Plotting the proportion of female laureates per decade
ggplot(prop_female_winners, aes(decade,proportion, color = Category)) + 
      geom_line() + geom_point() + scale_y_continuous( limits = c(0.0,1.0), 
      labels = scales::percent, expand = c(0,0)) 
#Category is mapped to color parameter to visualize all the categories in the graph



#6.First women to win the Nobel Prize 
nobel %>% filter(Sex == 'Female') %>% top_n(n = 1, wt = desc(Year)) %>% select(`Full Name`)



#7.Repeat Laureates
# Select the laureates that have received 2 or more prizes.
nobel %>%  group_by(`Full Name`) %>% count()  %>% filter(n>1)



#8.Age of the Nobel Laureates
nobel_age <- nobel %>% mutate( age = Year - year(`Birth Date`)) #function year() returns the year given a date
nobel_age



#9.Age trends within different prize categories
ggplot(nobel_age, aes(Year,age)) + geom_point() + geom_smooth(se = FALSE) +
                      facet_wrap(~Category) 
#geom_smooth uses argument (se = FALSE) to remove the confidence band
#results will be faceted by category



#10.Oldest and youngest winners

# The oldest winner of a Nobel Prize as of 2016
nobel_age %>% top_n(1, wt = age) %>% select(`Full Name`)

# The youngest winner of a Nobel Prize as of 2016
nobel_age %>% top_n(1, wt = desc(age)) %>% select(`Full Name`)
















