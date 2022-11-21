
# Pihla Norros 21Nov2022 

# Student Performance Data Set from UCI repository (source: Paulo Cortez, University of Minho, GuimarÃ£es, Portugal, http://www3.dsi.uminho.pt/pcortez)

# 1. Read both student-mat.csv and student-por.csv into R (from the data folder) 

student_por <- read.csv("C:/Users/T540p/OneDrive/Työpöytä/Data science 08Nov/IODS-project/data/student-por.csv", header = TRUE, sep = ",") 

student_mat <- read.csv("C:/Users/T540p/OneDrive/Työpöytä/Data science 08Nov/IODS-project/data/student-mat.csv", header = TRUE, sep = ",")

# Explore the structure and dimensions of the data.

dim(student_por) 
dim(student_mat)
structure(student_por)
structure(student_mat)

# access the dplyr package

library(dplyr)

# Check column names

colnames(student_mat)
colnames (student_por)

# Select the columns that vary in the two data sets

free_cols <- c("failures", "paid", "absences", "GI", "G2", "G3")

# remaining columns are common identifiers used for joining the data sets

join_cols <- setdiff(colnames(student_por), free_cols)

# Join the two data sets using all other variables than "failures", "paid", "absences", "G1", "G2", "G3" as (student) identifiers.

math_por <- inner_join(student_mat, student_por, by = join_cols, suffix = c(".math", ".por"))

# Explore the structure and dimensions of the joined data.

dim(mat_por)
structure(mat_por)

# Get rid of the duplicate records in the joined data set. 

# print out the column names of 'mat_por'

colnames(mat_por)


# create a new data frame with only the joined columns
alc <- select(mat_por, all_of(join_cols))

# print out the columns not used for joining (those that varied in the two data sets)

colnames(free_cols)

# for every column name not used for joining...

for(col_name in free_cols) {
  # select two columns from 'math_por' with the same original name
  two_cols <- select(mat_por, starts_with(col_name))
  # select the first column vector of those two columns
  first_col <- select(two_cols, 1)[[1]]
  
  
  # then, enter the if-else structure!
  # if that first column vector is numeric...
  if(is.numeric(first_col)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[col_name] <- round(rowMeans(two_cols))
  } else { # else (if the first column vector was not numeric)...
    # add the first column vector to the alc data frame
    alc[col_name] <- first_col
  }
}

# glimpse at the new combined data

glimpse(mat_por)

# Take the average of the answers related to weekday and weekend alcohol consumption
# to create a new column 'alc_use' to the joined data. 

alc_use = (Dalc + Walc) / 2


# Use 'alc_use' to create a new logical column 'high_use' which is TRUE for students for which 'alc_use' is greater than 2 (and FALSE otherwise). (1 point)

alc <- mutate(alc, alc_use = (Dalc + Walc) / 2) 
              

alc <- mutate(alc, high_use = if_else (alc_use > 2,
                                       
                                       TRUE,
                                       
                                       FALSE))

# Glimpse at the joined and modified data to make sure everything is in order. The joined data should now have 370 observations. 

glimpse(mat_por)
# Save the joined and modified data set to the ‘data’ folder,
# using for example write_csv() function

write_csv(mat_por)

