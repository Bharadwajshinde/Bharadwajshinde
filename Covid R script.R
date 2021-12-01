rm(list=ls()) #removes all variables stored previously
library(Hmisc)
data <- read.csv("C:/Users/admin/Desktop/Google Data Analytics/capstone project/Covid_R Project/COVID19_line_list_data.csv")
describe(data) #Hmisc command

# cleaned up death column
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# AGE
# claim: people who die are older (than who survived)
dead = subset(data, death_dummy == 1) # SUBSET FOR DEAD=1 & ALIVE=0
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE) # TO REMOVE NA FROM DATA AFFECTING MEAN 68.58
mean(alive$age, na.rm = TRUE) # TO REMOVE NA FROM DATA AFFECTING MEAN 48.07
# IS THIS STATISTICALLY SIGNIFICANT?
# H0: NULL HYPOTHESIS-IF age is not significant factor for death
# H1: ALTERNATE HYPOTHESIS age is significant factor for death

t.test(alive$age, dead$age, alternative= "two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant

# Gender
# claim: Gender has no effect
men = subset(data, gender == "male") # SUBSET FOR DEAD=1 & ALIVE=0
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) # TO REMOVE NA FROM DATA AFFECTING MEAN 8.46%
mean(women$death_dummy, na.rm = TRUE) # TO REMOVE NA FROM DATA AFFECTING MEAN 3.66%
# IS THIS STATISTICALLY SIGNIFICANT?
# H0: NULL HYPOTHESIS-IF gender is not significant factor for death
# H1: ALTERNATE HYPOTHESIS gender is significant factor for death
t.test(men$death_dummy, women$death_dummy, alternative= "two.sided", conf.level = 0.99)
# 99% confidence : men have from 0.8% to 8.8% higher chances of dying.
# normally, if p-value < 0.05, we reject null hypothesis
# here, p-value ~ 0, so we reject the null hypothesis and 
# conclude that this is statistically significant that men have higher death rate than women
