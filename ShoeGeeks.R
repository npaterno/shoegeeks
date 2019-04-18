#Running Shoe Analysis#
#Data collected via Google Forms Survey posted to the Running Shoe Geeks Facebook group (12000+ members).

library(tidyverse)
library(stringr)
library(hexbin)

#read in raw data; last update 4/17/19 8:30 pm
raw_data <- readr::read_csv("C:/Users/Guru/Documents/R/ShoeGeeks/Running Shoe Survey.csv")

#rename variables and drop the Timestamp from GoogleForms
data_new_names <- raw_data %>% 
  separate(`Shoe Size`, into = c("size","gender"),sep = " ") %>% 
  rename(experience = `How long have you been running? (number of years)`, 
         pairs_owned = `How many pairs of running shoes do you currently own?`,
         pairs_used = `How many pairs of running shoes are in your current rotation?`,
         adidas = `What brands are represented in your collection and how many pairs do you have for those brands? [Adidas]`,
         altra = `What brands are represented in your collection and how many pairs do you have for those brands? [Altra]`,
         asics = `What brands are represented in your collection and how many pairs do you have for those brands? [Asics]`,
         brooks = `What brands are represented in your collection and how many pairs do you have for those brands? [Brooks]`,
         hoka = `What brands are represented in your collection and how many pairs do you have for those brands? [Hoka]`,
         mizuno = `What brands are represented in your collection and how many pairs do you have for those brands? [Mizuno]`,
         new_balance = `What brands are represented in your collection and how many pairs do you have for those brands? [New Balance]`,
         newton = `What brands are represented in your collection and how many pairs do you have for those brands? [Newton]`,
         nike = `What brands are represented in your collection and how many pairs do you have for those brands? [Nike]`,
         on = `What brands are represented in your collection and how many pairs do you have for those brands? [On]`,
         salming = `What brands are represented in your collection and how many pairs do you have for those brands? [Salming]`,
         saucony = `What brands are represented in your collection and how many pairs do you have for those brands? [Saucony]`,
         skechers = `What brands are represented in your collection and how many pairs do you have for those brands? [Skechers]`,
         other = `What brands are represented in your collection and how many pairs do you have for those brands? [Other]`,
         race = `Do you participate in races?`,
         common_race = `If yes, what distance do you race most often?`,
         miles_per_week = `On average, how many miles per week do you run?`) %>% 
  select(-Timestamp)

#Correct data entries
#NEED TO: drop "pairs" and adjust race distances

#recode data to correct types
#NEED TO: brands->double, common_race->string
data_correct_type <- data_new_names %>% 
  mutate(size = as.numeric(size)) 
  
#split data by gender and plot histograms
#NEED TO: theme() and labs() graphs
men <- data_correct_type %>% 
  filter(gender=="M")

ggplot(men,aes(x=size))+
  +     geom_histogram(fill="blue",color="white",binwidth = 0.5)

women <- data_correct_type %>% 
  filter(gender=="W")

ggplot(women,aes(x=size))+
  +     geom_histogram(fill="blue",color="white",binwidth = 0.5)
