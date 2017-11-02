# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer

library(dplyr)


age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

height_diff <- round(students$height - mean(students$height), 2) * 100

(check_height_3 <- function(students){
  for (number_of_row in 1:nrow(students)){
    #calculate mean height of gender
    mean_height = as.numeric(students %>%
                               group_by(sex) %>%
                               summarise(mean(height)) %>%
                               filter(sex == students[number_of_row,"sex"]) %>%
                               select("mean(height)"))
    height_diff[number_of_row] <- round((students[number_of_row,"height"] - mean_height)*100, 2)
  }
  check_data_frame <- data.frame(students$name, height_diff)
  check_data_frame
})
check_height_3(students)
