library(tidyverse)
mean <- 70
stddev <- 5
 n <- 30
error <- qnorm(.95)*stddev
error

lower_bound <- mean - error
 lower_bound

 upper_bound <- mean + error
 upper_bound
 
mean <- 70
stddev <- 5
n <- 30
error1 <- qnorm(.99)*stddev
error1

lower_bound1 <- mean - error1
lower_bound1

upper_bound1 <- mean + error1
upper_bound1

stargazer(c(lower_bound, upper_bound),
type = 'latex')

stargazer(c(lower_bound1, upper_bound1),
          type = 'latex')


p1 <- ggplot(data = data.frame(x = c(60, 120)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 90, sd = 10)) + ylab("") +
  scale_y_continuous(breaks = NULL)+
  labs(x = 'Range',
       title = 'Distribution with a Mean of 90 and a SD of 10')
p1

p1 <- ggplot(data = data.frame(x = c(55, 85)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 70, sd = 5)) + ylab("") +
  scale_y_continuous(breaks = NULL)+
  labs(x = 'Range',
       title = 'Distribution with a Mean of 70 and a SD of 5')
p1

mtcars<-mtcars

library(janitor)
Freq_Table<-tabyl(mtcars$carb)

stargazer(Freq_Table,
          type = 'latex',
          summary = FALSE)

# how to make frequency table in r (nicer version)
install.packages('epiDisplay')
library(epiDisplay)
tab1(mtcars$carb, sort.group = "decreasing", cum.percent = TRUE)



data("CASchools")

ggplot()+
  geom_point(data = CASchools,
             aes(x =income, y = read ))+
  labs(x = 'Income in Thousands',
       y = 'Reading Scores',
       title = 'CA Schools Reading Plotted Against Income',
       caption = 'Hey Dr. McNab I have come a long way in coding this was \n not as bad as last year')

stargazer(CASchools,
          type = 'latex',
          summary = TRUE)
CASchools$Lowincome<-(CASchools$income  <= mean(CASchools$income))
Lowincome<-(CASchools,ASchools$income  <= mean(CASchools$income))
stargazer(Lowincome,
          type = 'latex',
          summary = TRUE)
ggplot(data=CASchools, aes(x=Lowincome, y = 'count')) +
  geom_bar(stat="identity")+
  labs(x = 'Is Household Income Lower Than \n the Sample Average',
       y = 'Count',
       title = 'Frequecy of Low Income Students')
     

