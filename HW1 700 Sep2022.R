rm(list = ls())

library(kableExtra)
library(knitr,quietly = TRUE)
library(AEP)
library(tinytex)
library(stargazer)
library(dplyr)
library(tidyverse)
#########Problem 1############

x1 <- 32*506;
x2 <- 1245/23;
x3 <- x1*x2;
x4 <- c(x1,x2,x3);

vector_1 <- c(2,5,12,16);
vector_2 <- c(5,6,7,8);
vector_1 * vector_2;

difference<-vector_1 - vector_2

stargazer(difference,
          type = 'latex')
vector_horse_color <- c('red','blue','orange','purple','pink')
vector_horse_weight <- c(1250,1354,1159,1532,1642)

horse_all <- cbind(vector_horse_color, vector_horse_weight);
  stargazer(horse_all,
        type = 'latex')

########### Problem 2##############
library(censusapi, quietly = TRUE)
library(tidycensus, quietly = TRUE)


if (FALSE) {
  census_api_key("111111abc", install = TRUE)
  # First time, reload your environment so you can use the key without restarting R.
  readRenviron("~/.Renviron")
  # You can check it with:
  Sys.getenv("99b35bdaf7863a958eac3b4d521cddb7315f6479")
}
va_data <- getCensus(name = "pep/population",
                     vintage = 2019,
                     key = Sys.getenv("CENSUS_API_KEY"),
                     vars = c("NAME",
                              "POP",
                              "DATE_CODE"),
                     region = "COUNTY:*",
                     regionin = "STATE:*")
raw_example2 <- va_data %>%
  filter(state == "51" & DATE_CODE == 12 & POP > 50000)

stargazer(raw_example2,
          type = 'latex',
          summary = FALSE)
#############Question 5/6############
va_data <- getCensus(name = "pep/population",
                     vintage = 2019,
                     key = Sys.getenv("CENSUS_API_KEY"),
                     vars = c("NAME",
                              "POP",
                              "DATE_CODE"),
                     region = "COUNTY:*",
                     regionin = "STATE:*")
raw_example4 <- va_data %>%
  
  filter(state == "51" & DATE_CODE == 12)
stargazer(nrow(raw_example4))
########### Question 7:###########
va_data <- getCensus(name = "pep/population",
                     vintage = 2019,
                     key = Sys.getenv("CENSUS_API_KEY"),
                     vars = c("NAME",
                              "POP",
                              "DATE_CODE"),
                     region = "COUNTY:*",
                     regionin = "STATE:*")
raw_example5 <- va_data %>%
  
  filter(state == "51" & DATE_CODE == 12) %>%
  summarise(min = min(POP),
            max = max(POP), 
            median = median(POP), 
            mean = mean(POP),
            Std = sd(POP));

stargazer(raw_example5,
      type = 'latex'
      )
print(nrow())
#########Problem 3########

county_pop <- va_data %>%
  filter(DATE_CODE != 1 & DATE_CODE != 2) %>%
  mutate(name = NAME,
         total_pop = POP,
         fips = paste0(state, county),
         DATE_CODE = as.numeric(DATE_CODE)) %>%
  mutate(year = recode(DATE_CODE,
                       `3` = 2010,
                       `4` = 2011,
                       `5` = 2012,
                       `6` = 2013,
                       `7` = 2014,
                       `8` = 2015,
                       `9` = 2016,
                       `10`= 2017,
                       `11`= 2018,
                       `12`= 2019)) %>%
  select(fips, year, name, total_pop, state, county) %>%
  filter(state == '51') %>%
  filter(year == 2019);
anythingisfine <- county_pop %>% 
  filter(year == 2019) %>% 
  summarise(min = min(total_pop),
            max = max(total_pop), 
            median = median(total_pop), 
            mean = mean(total_pop),
            Std = sd(total_pop));
anythingisfine;
#Descriptive Stats
stargazer(anythingisfine, 
type = 'latex',
summary = TRUE)

# Min Max Table
county_minmax <- county_pop %>%
  filter(total_pop == max(total_pop) | total_pop == min(total_pop))
stargazer(county_minmax,
          type = 'latex',
          summary = FALSE)


Mean_By_Year <- va_data%>%
  filter(state == '51', DATE_CODE != 1 & DATE_CODE!= 2)%>%
  group_by(DATE_CODE)%>%
  summarise(mean(POP))

as.numeric(Mean_By_Year$DATE_CODE)

Greater_Than<- county_pop %>%
  filter(total_pop >= mean(total_pop))
