#Pinja Marin, 12.11.20, creating the dataset "alc".
#Data available here, in the data folder: https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(dplyr)

setwd('./data') 

student_por <- read.table('student-por.csv', sep = ";" , header=TRUE)
student_mat <- read.table('student-mat.csv', sep = ";", header=TRUE)

dim(student_por) #649 observations, 33 variables
str(student_por) # variables are characters and integers

dim(student_mat) #395 obs, 33 var
str(student_mat) # variables are characters and integers

# columns that are used to join the datasets
join_by <- c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery","internet")

mat_por <- inner_join(student_mat, student_por, by = join_by, suffix = c(".mat", ".por"))

mat_por #I realize there are too many individuals now... (the forum comment, got too little observations when trying to follow the GitHub code)
dim(mat_por) #382 obs, 53 var.
str(mat_por)

#combining the 'duplicated' answers in the joined data (based on the DataCamp solution)
#1) creating a data frame with only the joined columns
alc <- select(mat_por, one_of(join_by))

# columns that were not used for joining the data
varying_cols <- colnames(student_mat)[!colnames(student_mat) %in% join_by]

# for every column name not used for joining
for(column_name in varying_cols) {
  # select two columns with the same original name
  two_columns <- select(mat_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  # if that first column  vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

#Taking the average of the answers related to weekday and weekend alcohol consumption, and adding it as a new column
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

#Alc_use is high_use when:
alc <- alc%>%
mutate(high_use = alc_use > 2)

glimpse(alc) #rows 382, cols 35. As mentioned earlier, too many observations... Variables seem to be fine.

#saving data into the data folder
write.csv(alc, file='alc.csv')


