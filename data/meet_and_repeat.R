#Pinja Marin
#3.12.20
#data wrangling for two longitudinal datasets, i.e. datasets with repeated measures 

#loading some libraries
library(dplyr)
library(tidyr)

#loading the BPRS dataset (in wide form) to R:
BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt",sep  =" ",header=TRUE)

#loading the RATS dataset (in wide form) to R:
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt",sep  ="\t",header=TRUE)

#Taking a look at the BPRS data:
names(BPRS)
dim(BPRS)#40 obs, 11 vars.
str(BPRS) #id length is 20 because the same id numbers are used in different groups, i.e. for different individuals.
BPRS #printing the whole data: 40 individuals, 2 treatment groups
summary(BPRS) #quite a lot of variety in the measurements taken each week

#Taking a look at the RATS data:
glimpse(RATS) #13 vars, 16 obs.
RATS #printing the data: 16 rats, 3 groups.
summary(RATS) #quite a lot variety in the weight measures

#in both datasets all variables are type integer, i.e. numerical. Let's convert the categorical variables of both data sets to factors. 
#
#the ID and Group variables to factors in the RATS data:
RATS$ID <- factor(RATS$ID)
RATS$Group <- factor(RATS$Group)
glimpse(RATS) #looks good.

#the treatment and subject variables to factors in the BPRS data:
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)
str(BPRS) #looks good.

#Converting the data sets to long form
BPRS_l <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)

head(BPRS_l,21)
tail(BPRS_l,21) #looks good.

RATS_l <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) 

head(RATS_l,21)
tail(RATS_l,21) #looks good.

#Add a week variable to BPRS
BPRS_l <- BPRS_l %>% mutate(week = as.integer(substr(weeks,5,5)))

glimpse(BPRS_l)
BPRS_l$week

#add a Time variable to RATS. 
RATS_l <- RATS_l %>% mutate(Time = as.integer(substr(WD,3,4))) 

glimpse(RATS_l)
RATS_l$Time

#Taking a further look at the long form datasets

names(RATS_l)
glimpse(RATS_l)
summary(RATS_l)
head(RATS_l,20)
RATS_l #ID and group numbers are repeated so that each row shows one rats' measurement time and that time's value.

names(BPRS_l)
summary(BPRS_l)
dim(BPRS_l) #360 rows, 5 vars i.e. columns.
str(BPRS_l)
head(BPRS_l,30) #subject and treatment values are repeated so that each row shows one individuals' measurement time and that time's value

#Compared to the above described long form, in the wide data formats each row contains all the measurement values for one individual/rat, in multiple columns (as was seen in the beginning of this syntax)

#saving the datas into the data folder
setwd("./data")
write.csv(RATS_l, file='RATS_l.csv')
write.csv(BPRS_l, file='BPRS_l.csv')
