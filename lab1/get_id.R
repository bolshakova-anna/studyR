
get_id <- function(all_data){
  merged <- Reduce(function(x, y) merge(x, y, by = 'id'), all_data) #объединение листа df по id (+ решает проблему проверки того, что человек д.б. во всех днях)
  means_temp <- rowMeans(merged[-which(colnames(merged) == 'id')])  
  res <- data.frame(id = merged[which(colnames(merged) == 'id')], mean_temp = means_temp)
  return(res)
}

#применение
setwd(getwd())
file_name <- "C:/Users/User/studyR/data/lab1_e2.Rdata"
data <- load(file_name)
res = get_id(all_data)
print(res)
