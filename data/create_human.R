#Pinja Marin
#creating the human data through combining two datasets, and making some modifications 
#to the data

#Read the “Human development” (hd) and “Gender inequality” (gi) datas into R

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)

gi <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#More information about the datasets:
# http://hdr.undp.org/en/content/human-development-index-hdi # metadata
# http://hdr.undp.org/sites/default/files/hdr2015_technical_notes.pdf #technical information

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

#First, creating the ratio of Female and Male populations with secondary education 
#in each country.
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

glimpse(human) #195 rows, 19 columns (as was supposed to)

#saving the data into my data folder (which is the same directory as this file)
write.csv(human, file='human.csv')



#continuing data wrangling during week5
# I am using the data I created last week but the combined data is also available here: 
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt

#load data into R
human2 <- read.csv("human.csv")
dim(human2) #20 columns, should be 19, 195 observations which is correct
glimpse(human2)
head(human2,4) #there is an additional x-column of row numbers, let's delete it.
human2 <- human2[, 2:20]
dim(human2) #195,19
str(human2) # there are 19 variables. Except for the country variable, all variables 
#give numerical information about the coutries (and areas) on matters such as 
#equality and development. The variable GNI (Gross National Income per capita) 
#appears to be in a character form although it is supposed to be a numerical value. 
#Besides GNI, some example variables are "life_exp" = Life expectancy at birth, 
#and "Edu2_f" = Proportion of females with at least secondary education. 

human2$country #this variable mainly refers to different countries but there also 
#some larger areas such as "Europe and Central Asia" and even "World". 
#More information about the dataset can be found earlier in this script, in the two 
#links from last week.

#Let's transfrom the GNI to numeric.
# first, accessing the stringr package
library(stringr)

str(human2$GNI) #there are unwanted commas after thousands

# Let's remove the commas from GNI and save a numeric version of it to the data
human2$GNI <- str_replace(human2$GNI, pattern=",", replace ="") %>% as.numeric()

str(human2$GNI) #seems good.

#Let's exclude unneeded variables and keep only these:
keep <- c("country", "edu2_ftom", "labour_ftom", "Edu_exp", "life_exp", "GNI", 
          "mat_mor", "Adol_birth", "parl_f")

# selecting the 'keep' columns
human_s <- select(human2, one_of(keep))
dim(human_s) #9 variables, as wanted

#Let's remove all rows with missing values

# first, printing out the data along with a completeness indicator as the last column 
# (it shows whether there are any missing values per row)
data.frame(human_s[-1], comp = complete.cases(human_s))

# filtering out the rows with NA values
human_f <- filter(human_s, complete.cases(human_s))
dim(human_f) #162 rows now

#Remove the observations which relate to regions instead of countries.
human_f$country #it seems that the last 7 observations are not countries but 
#regions. Let's remove them. 

# defining the last row we want to keep
last <- nrow(human_f) - 7

# choosing everything until the last 7 observations
humanf <- human_f[1:last,]

dim(humanf) #155 rows, seems correct.

#Define the row names of the data by the country names 
rownames(humanf) <- humanf$country

#remove the country name column from the data.
humanf <- select(humanf, -country)

str(humanf) #155 obs, 8 var. This is correct.
head(humanf,3)

#Save the human data in your data folder including the row names. 
#Let's not overwrite the old ‘human’ data in case there are any issues and 
#instead keep this data as 'humanf'.
write.csv(humanf, file='humanf.csv')
