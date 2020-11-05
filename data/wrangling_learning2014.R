#Pinja Marin, 5.11.20
#wrangling the learning2014 data set into usable form

library(dplyr)

lrndata <-read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)

dim(lrndata) # 183 participants , 60 variables
str(lrndata) #All variables are integers except gender is a character

summary(lrndata$Attitude)

#defining the columns

deep <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surf <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
stra <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

#creating sumvariables
deep_col <- select(lrndata, one_of(deep))
lrndata$deep <- rowMeans(deep_col)

surf_col <- select(lrndata, one_of(surf))
lrndata$surf <- rowMeans(surf_col)

stra_col <- select(lrndata, one_of(stra))
lrndata$stra <- rowMeans(stra_col)

#selecting the wanted variables

to_anadata <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
#creating a dataset with these values
anadata <- select(lrndata, one_of(to_anadata))

anadata$Points

#excluding the observations where points is zero.
finaldata <- subset(anadata, (Points > 0))
finaldata$Points
dim(finaldata) #166 obs, 7 var

setwd("~/GitHub/IODS-project/data") #added the /data so file will be saved here when write.csv

write.csv(finaldata, file='learning2014.csv')

#reading and saving the file into learning2014
lrn2014<- read.csv('learning2014.csv') #X=id number now in the data

glimpse(lrn2014)
str(lrn2014)
head(lrn2014)
