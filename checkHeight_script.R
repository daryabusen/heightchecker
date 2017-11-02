# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer
# Editor: Darya Busen


#takes data frame as input and returns a data frame in which the name of the students 
#and the calculates gender specific height difference for each students.

result.frame = data.frame(matrix(NA, nrow = nrow(students.input), ncol = 2))
colnames(result.frame) = c("name", "difference")

 # via pipe operator

  male.mean = students.input %>%
  filter(sex == "M") %>%
  summarise(mean = mean(height))
female.mean = students.input %>%
  filter(sex == "F") %>%
  summarise(mean = mean(height))

# calculates sex specific height difference for each row(student) of student
# data.frame
  height.vector = apply(students.input, MARGIN = 1, 
        FUN = function(student){
        return( (if (student["sex"] == "M") male.mean - as.numeric(student["height"]) 
                                                                                                                                         else female.mean - as.numeric(student["height"]) ) )
                                                                        } )
# fills output data.frame with names and calculated height differences
  result.frame$name = students.input$name
  result.frame$difference = height.vector
  
 # return output data.frame
  return(result.frame)



  checkHeight3(students)
