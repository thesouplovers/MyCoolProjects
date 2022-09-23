rm(list = ls())
# Load Library
install.packages("e1071")
library(e1071)
library(readxl)
library(tidyr)
library(writexl)
library(ggplot2)
library(dplyr)
library(stargazer)
library(lmtest)
library(MASS)
library(sandwich)
library(OECD)
library(kableExtra)
library(tidyverse)
library(stringr)
library(scales)
install.packages("miscTools")
install.packages("plm", dependencies = TRUE)
install.packages("rbibutils",  type="source")
library(plm)
library(rbibutils)
# Loading Data Packs

Union_Density <- read.csv("~/Documents/706 Data Files/TUD_28032022235020223.csv") %>%
filter(Time > 1999)


OECD_DATA <- read.csv("~/Documents/706 Data Files/OECD-AIAS-ICTWSS-CSV.csv", header=TRUE)%>%
  group_by(country) %>% 
  filter(UD != -88 & year > 2008)

Inequality_GINI <- read.csv("~/Downloads/DP_LIVE_07042022181202080.csv")


GDP_Capita <- read.csv("~/Downloads/DP_LIVE_07042022210240781.csv")%>%
rename( GDP_Per_Capita = Value)

# Merge Data Sets

Inequality_and_Density <- merge(Inequality_GINI, Union_Density, by = c('TIME', 'LOCATION')) 
Inequality_and_Density <- merge(Inequality_and_Density, GDP_Capita, by = c('TIME', 'LOCATION'))
# Descriptive Stats
hist(Inequality_and_Density$Value.y)
skewness(Inequality_and_Density$Value.y)

Descriptive_Stats <- Inequality_and_Density %>%
  rename("GINI Coeffecient" = Value.x,
         "Union Density" = Value.y,
        'GDP Per Capita' = GDP_Per_Capita)%>%
dplyr::select("Union Density",
         "GINI Coeffecient",
         'GDP Per Capita')

#Output for Descriptive Stats
table_1 <- stargazer(Descriptive_Stats,
                     type = "latex",
                     title = 'Table of Descriptive Statistics for Relevant Data & Union Density',
                     summary = TRUE,
                     no.space = TRUE, # to remove the spaces after each line of coefficients
                     summary.stat = c('mean', 'sd',  'min', 'median', 'max'),
                     column.sep.width = "3pt", # to reduce column width
                     font.size = "small", # to make font size smaller
                     out = "~/Downloads/summarystats.html")

#Displaying Countries used in the Model
Countries<-stargazer(unique(Inequality_and_Density$Country),
                     type = 'latex', 
                     title = "Countries",
                     summary = FALSE,
                     single.row = FALSE,
                     out = "~/Downloads/Countries.html" )
all(is.na(Inequality_and_Density$Value.x))
all(is.na(Inequality_and_Density$Value.y))
all(is.na(Inequality_and_Density$GDP_Per_Capita))
Panel = pdata.frame(Inequality_and_Density, index = c("Country", "TIME"))
RandomModel = plm(Value.x~Value.y, data = Panel, model = "random")
firstdiffModel<- plm(Value.x~Value.y, data = Panel, model = "fd")
fixedModel<- plm(Value.x~Value.y, data = Panel, model = "within")
summary(fixedModel)
  
  stargazer(fixedModel,
            header = FALSE,
            align = TRUE,
            type = "text",
            dep.var.labels = c("Inequality"),
            covariate.labels = c("Union Density"),
            summary = FALSE,
            out = "~/Downloads/Model1.html")
  #Determining Wether to Used Fixed Effects or Random Effects
  phtest(RandomModel, fixedModel)
  #Adding Lag
  fixedModelwithlag<- plm(Value.x~lag(Value.x)+Value.y, data = Panel, model = "within")
stargazer( fixedModel,
                        header = FALSE,
                        align = TRUE,
                        type = "text",
                        dep.var.labels = c("Inequality"),
                        covariate.labels = c("Inequality Lag",
                                             "Union Density"),
                        summary = FALSE,
                        out = "~/Downloads/Model1.html")

vcov_1 <-coeftest(fixedModel,vocvHC(fixedModel,type = "HC0", cluster = 'group'))
  # Is the dependent variable following a normal distribution?
  hist(Inequality_GINI$Value)
  #No Autocorrelation
 
  # Is the relationship between Inequality and Density linear?
  plot(Value.y ~ Value.x, data = Inequality_and_Density)
  
  #Does the assumption of linearity hold?
  plot(fixedModel,1)

  #correlation between errors and x
  cor(Inequality_and_Density$Value.y,Model1$residuals)
  
  #homoscedasticity
  bptest(fixedModel)
  #Correcting Standard Errors
  vcov_1 <- vcovHC(Model1,type = "HC1")
  model_1_robust <- coeftest(Model1, vcov = vcov_1)
  model_1_robust.se <- sqrt(diag(vcov_1))
  

  #error here cor(Model1$coefficients, Model1$residuals)
  par(mfrow = c(2, 2))
  plot(fixedModel)
  

  #Inequality Plot over time
  ggplot(data = Inequality_and_Density,
         aes(x = Time, y = Value.x, group = Country)) +
    geom_point(aes(x = Time, y = Value.x, color = Country)) +
    geom_line() +
    labs(title = "GINI Index Scatterplot \n for 37 OECD Countries Over Time",
         x = "Time",
         y = "GINI Index",
         caption = "Source: Organization for Economic Cooperative Development \n Years:2000 - 2020")
  ggsave("regression.png", device = "png")
  #Density over time
  ggplot(data = Inequality_and_Density,
         aes(x = Time, y = Value.y, group = Country)) +
    geom_point(aes(x = Time, y = Value.y, color = Country)) +
    geom_line() +
    labs(title = "Union Density Scatterplot \n for 37 OECD Countries Over Time",
         x = "Time",
         y = "Union Density",
         caption = "Source: Organization for Economic Cooperative Development \n Years:2000 - 2020")
  ggsave("regression1.png", device = "png")

  stargazer( vcov_1,
            header = FALSE,
            align = TRUE,
            type = "text",
            notes = "Robust standard errors in parenthesis.",
            dep.var.labels = c("Income Inequality"),
            covariate.labels = c("Union Density"),
            se=list(model_1_robust.se),
            summary = FALSE,
            out = "~/Downloads/modelsrobust.html")
  
#Additional Graphs for the Paper
#Ghent System

Ghent_System <- read.csv("~/Downloads/TUD_09042022020110572.csv")

ggplot(data = Ghent_System,
    aes(x = Time, y = Value, group = Country)) +
      geom_line(aes(x = Time, y = Value, color = Country))+
 scale_x_continuous(breaks = seq(2000, 2020 , by = 2)) +
  labs(title = " The Leveling Off of Union Density \n Under the Ghent System During the \n 2008 Recession  ",
       x = "Years",
       y = "Union Density",
       caption = "Source: \n Organization for Economic Cooperative Development \n Years 2000 - 2020 ")
ggsave("plotGhent.png", device = "png")


Ghent_SystemDecrease <- Ghent_System %>%
  group_by(Country) %>% 
  summarise(Percent_Change = Value[TIME == 2019]-max(Value))

stargazer( fixedModel,
           header = FALSE,
           align = TRUE,
           type = "latex",
           notes = "Robust standard errors in parenthesis.",
           dep.var.labels = c("Income Inequality"),
           covariate.labels = c("Union Density"),
           se=list(model_1_robust.se),
           summary = FALSE,
           out = "~/Downloads/modelsrobust.html")





