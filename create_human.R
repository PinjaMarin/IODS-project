#Read the “Human development” (hd) and “Gender inequality” (gi) datas into R

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gi <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#More information about the datasets:
http://hdr.undp.org/en/content/human-development-index-hdi # metadata
http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf #technical information

#Exploring the datasets
dim(hd) # 195 obs, 8 var
str(hd) # gross national income is a character, although it contains numbers
summary(hd) #big range on many variables
#let's take a look at the country variable
hd$Country #195 countries, each county its own row (observation) - although last row is "world".

dim(gi) #195 obs, 10 var
str(gi) #only character variable is country
summary(gi) #big range on many variables
gi$Country

head(gi$Country,10) #possibly to be the same countries
head(hd$Country,10)

#loading packages
library(dplyr)
library(tidyverse)
library(ggplot2)

#Renaming the variables
hd_f <- hd %>%
  rename(country=Country, HDI_rank=HDI.Rank, HDI=Human.Development.Index..HDI., life_exp = Life.Expectancy.at.Birth, Edu_exp = Expected.Years.of.Education, Edu_mean = Mean.Years.of.Education, GNI=Gross.National.Income..GNI..per.Capita, Rank_GNI_HDI=GNI.per.Capita.Rank.Minus.HDI.Rank)

glimpse(hd_f) #seems good

gi_f <- gi %>%
  rename(GII_rank=GII.Rank, country=Country, GII=Gender.Inequality.Index..GII., mat_mor=Maternal.Mortality.Ratio, Adol_birth=Adolescent.Birth.Rate, parl_f=Percent.Representation.in.Parliament, edu2_f=Population.with.Secondary.Education..Female., edu2_m=Population.with.Secondary.Education..Male., labour_f=Labour.Force.Participation.Rate..Female., labour_m=Labour.Force.Participation.Rate..Male.)

glimpse(gi_f) #seems good.

#Mutating the “Gender inequality” data to create two new variables. 

#First, creating the ratio of Female and Male populations with secondary education in each country.
gi_f <- gi_f %>%
  group_by(country) %>%
  mutate(edu2_ftom = edu2_f / edu2_m) %>%
  ungroup ()

summary(gi_f$edu2_ftom) #median 0.93

#Second, the ratio of labour force participation of females and males in each country.
gi_f <- gi_f %>%
  group_by(country) %>%
  mutate(labour_ftom = labour_f / labour_m) %>%
  ungroup ()

summary(gi_f$labour_ftom) #median 0.75

#Joining together the two datasets using country as the identifier.
#calling the new data "human".
human <- inner_join(hd_f, gi_f, by = "country", suffix = c(".hd", ".gi"))

glimpse(hd_gi) #195 rows, 19 columns (as was supposed to)

#saving the data into my data folder
#first, setting the correct working directory
setwd("~/GitHub/IODS-project/data") 
#then saving the data
write.csv(human, file='human.csv')




