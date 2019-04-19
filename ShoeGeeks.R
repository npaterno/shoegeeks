#Running Shoe Analysis#
#Data collected via Google Forms Survey posted to the Running Shoe Geeks Facebook group (12000+ members).

library(tidyverse)
library(stringr)
library(scales)

#read in raw data; last update 4/18/19 am (n=155)
raw_data <- readr::read_csv("https://raw.githubusercontent.com/npaterno/shoegeeks/master/Running%20Shoe%20Survey.csv?token=ALTXHMHABANBTVDRZG4YMOC4XEZ4I")

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
#Uncommon distances like 12k, 20k rounded to closest major distance 
data_new_entries <- data_new_names %>% 
  separate(adidas, into = c("Adidas","drop1"),sep = " ") %>% 
  separate(altra, into = c("Altra","drop2"),sep = " ") %>% 
  separate(asics, into = c("Asics","drop3"),sep = " ") %>% 
  separate(brooks, into = c("Brooks","drop4"),sep = " ") %>% 
  separate(hoka, into = c("Hoka","drop5"),sep = " ") %>% 
  separate(mizuno, into = c("Mizuno","drop6"),sep = " ") %>% 
  separate(new_balance, into = c("New Balance","drop7"),sep = " ") %>% 
  separate(newton, into = c("Newton","drop8"),sep = " ") %>% 
  separate(nike, into = c("Nike","drop9"),sep = " ") %>% 
  separate(on, into = c("On","drop10"),sep = " ") %>% 
  separate(salming, into = c("Salming","drop11"),sep = " ") %>% 
  separate(saucony, into = c("Saucony","drop12"),sep = " ") %>% 
  separate(skechers, into = c("Skechers","drop13"),sep = " ") %>% 
  separate(other, into = c("Other","drop14"),sep = " ") %>% 
  select(-c("drop1","drop2","drop3","drop4","drop5","drop6","drop7",
            "drop8","drop9","drop10","drop11","drop12","drop13","drop14")) %>% 
  mutate(distance = case_when(
    common_race %in% c("5k","5K","5","5000m") ~ "5 K",
    common_race %in% c("10k","10K") ~ "10 K",
    common_race %in% c("Triathlon. 13.1", "21km","Half Marathons","Half","13.1 Miles","Half marathon","Half Marathon","half marathon","half-marathon","13.1")  ~ "Half Marathon",
    common_race %in% c("Marathon","42","42k","Marathon (Ironman)", "marathon", "Full marathon", "26.2") ~ "Marathon",
    common_race %in% c("50-70 km","26.2+", "50 mile", "Ultra", "Ultras","Ultra distance of varying length", "50k" ) ~ "Ultra",
    is.na(common_race) ~ "None",
    TRUE ~ "Other"
    ))

#recode data to correct types
#possibly a faster/more efficient method? 
data_clean <- data_new_entries %>% 
  select(-common_race) %>% 
  mutate(size = as.numeric(size)) %>% 
  mutate(Adidas = as.numeric(Adidas)) %>% 
  mutate(Altra = as.numeric(Altra)) %>% 
  mutate(Asics = as.numeric(Asics)) %>% 
  mutate(Brooks = as.numeric(Brooks)) %>% 
  mutate(Hoka = as.numeric(Hoka)) %>% 
  mutate(Mizuno = as.numeric(Mizuno)) %>% 
  mutate(`New Balance` = as.numeric(`New Balance`)) %>% 
  mutate(Newton = as.numeric(Newton)) %>% 
  mutate(Nike = as.numeric(Nike)) %>% 
  mutate(On = as.numeric(On)) %>% 
  mutate(Salming = as.numeric(Salming)) %>% 
  mutate(Saucony = as.numeric(Saucony)) %>% 
  mutate(Skechers = as.numeric(Skechers)) %>% 
  mutate(Other = as.numeric(Other)) 

#split data by gender 
men <- data_clean %>% 
  filter(gender=="M")

women <- data_clean %>% 
  filter(gender=="W")

#Create data set to compare brand popularity/loyalty across gender
brand_pop <- data_clean %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(
            Adidas = sum(!is.na(Adidas)),
            Altra = sum(!is.na(Altra)),
            Asics = sum(!is.na(Asics)),
            Brooks = sum(!is.na(Brooks)),
            Hoka = sum(!is.na(Hoka)),
            Mizuno = sum(!is.na(Mizuno)),
            `New Balance` = sum(!is.na(`New Balance`)),
            Newton = sum(!is.na(Newton)),
            Nike = sum(!is.na(Nike)),
            On = sum(!is.na(On)),
            Salming = sum(!is.na(Salming)),
            Saucony = sum(!is.na(Saucony)),
            Skechers = sum(!is.na(Skechers)),
            Other = sum(!is.na(Other))) %>% 
  gather(Adidas, Altra, Asics, Brooks, Hoka, Mizuno, `New Balance`, Newton, Nike,
         On, Salming, Saucony, Skechers, Other, key = "brand", value = "runners")  
  


brand_loyal <- data_clean %>% 
  filter(!is.na(gender)) %>% 
  group_by(gender) %>% 
  summarize(Adidas = mean(Adidas,na.rm=TRUE),
            Altra = mean(Altra,na.rm=TRUE),
            Asics = mean(Asics,na.rm=TRUE),
            Brooks = mean(Brooks,na.rm=TRUE),
            Hoka = mean(Hoka, na.rm=TRUE),
            Mizuno = mean(Mizuno, na.rm=TRUE),
            `New Balance` = mean(`New Balance`,na.rm=TRUE),
            Newton = mean(Newton,na.rm=TRUE),
            Nike = mean(Nike,na.rm=TRUE),
            On = mean(On,na.rm=TRUE),
            Salming = mean(Salming,na.rm=TRUE),
            Saucony = mean(Saucony,na.rm=TRUE),
            Skechers = mean(Skechers,na.rm=TRUE)) %>% 
           # Other = mean(Other,na.rm=TRUE)) %>% 
  gather(Adidas, Altra, Asics, Brooks, Hoka, Mizuno, `New Balance`, Newton, Nike,
         On, Salming, Saucony, Skechers, key = "brand_avg", value = "per_runner")
  
##GRAPHICS BELOW##

#Age Histogram
ggplot(data_clean)+
  geom_histogram(aes(x=Age,y=stat(width*density)),fill="Dark Green",color="White",binwidth=5, na.rm=TRUE)+
  scale_y_continuous(label = percent_format())+
  labs(title="Age Distribution",
       x="Age",
       y="Percent")

#MPW Histogram
ggplot(data_clean)+
  geom_histogram(aes(x=miles_per_week,y=stat(width*density)),fill="Dark Green",color="White",binwidth=5, na.rm=TRUE)+
  scale_y_continuous(label = percent_format())+
  labs(title="Weekly Mileage Distribution",
       x="Miles Per Week",
       y="Percent")

#Brand Popularity Across Gender: Number of runners who own each brand
ggplot(brand_pop ,aes(x=brand,y=runners),color="white")+
  geom_col(fill="Dark Red")+
  coord_flip()+
  theme_bw() +
  facet_wrap(~gender)+
  labs(title="Brand Popularity",
       x="Brand",
       y="Number of Runners")

#Brand Loyalty: Average Pairs per runner per brand
ggplot(brand_loyal ,aes(x=brand_avg,y=per_runner),color="white")+
  geom_col(fill="Dark Red")+
  coord_flip()+
  theme_bw() +
  facet_wrap(~gender)+
  labs(title="Brand Loyalty",
       x="Brand",
       y="Average Pairs Per Runner")

#Scatterplot of Shoes Owned V Shoes Used
#Conclusion: most people run in less than half the shoes they own
ggplot(data_clean,aes(x=pairs_owned,y=pairs_used,color=gender))+
  geom_point(alpha=0.5)+
  geom_abline(slope = 0.5, intercept = 2, linetype = 2)+
  labs(title = "Collection Size V Rotation Size",
       x="Colletion Size",
       y="Rotation Size",
       color = "Gender")

#Shoe Size Histograms by Gender
ggplot(women,aes(x=size))+
  geom_histogram(aes(y=stat(width*density)),fill="red",color="white", binwidth=0.5)+
  scale_y_continuous(label = percent_format())+
  theme_bw()+
  labs(title="Women's Shoe Size",
       x="Size",
       y="Frequency")

ggplot(men,aes(x=size))+
  geom_histogram(aes(y=stat(width*density)),fill="blue",color="white", binwidth=0.5)+
  scale_y_continuous(label = percent_format())+
  theme_bw()+
  labs(title="Men's Shoe Size",
       x="Size",
       y="Frequency")