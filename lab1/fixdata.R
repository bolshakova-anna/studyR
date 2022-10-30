fix_data <- function(input){
  #считаем что один столбец - один формат
  #если после удаления пробелов дата стала числом то замена
  new_elem = as.numeric(gsub(' ', '', input))
  if (!any(is.na(new_elem))){
    input = new_elem
  }
  return(input)
}

#применение
file_name <- "C:/Users/User/studyR/data/lab1_e1.csv" 
setwd(getwd())
input <- read.csv(file_name)
output <- (apply(input,2,fix_data))

write.csv(output, file = 'output.csv', row.names = FALSE)
print(output)