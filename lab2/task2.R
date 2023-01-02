# library(ggplot2)
# library(magrittr)
# library(readr)
# library(tidyr)
# library(data.table)
# library(repr)
# library(stringr)



library(dplyr)
library(tibble)



# Реализация функции из задания 2:
#     Напишите функцию, в которой пользователь подает на вход название субъекта федерации или федерального округа 
#     и получает на выходе получает два списка: 
#     в первом списке названия тех отраслей промышленности, по которым экспорт превышает импорт, 
#     а во втором - названия тех отраслей промышленности, по которым импорт превышает экспорт.


get_lists_from_data <- function(input){
  region <- tibble(ExpImp[grep(input, ExpImp$Регион),])  #поиск input в таблице
  
  exp <- region[grep('.*Экспорт', colnames(region))] # только экспорт из input 
  colnames(exp) <- gsub("Экспорт","",colnames(exp)) # удаление из названий столбцов "Экспорт", т.е. ПродЭкспорт->Прод и т.п. 
  exp_numeric <- as.numeric(exp) #перевод значений показателей в числовой
  
  imp <- region[grep('.*Импорт', colnames(region))] # только импорт из input 
  colnames(imp) <- gsub("Импорт","",colnames(imp)) # удаление из названий столбцов "Импорт", т.е. ПродИмпорт->Прод и т.п. 
  imp_numeric <- as.numeric(imp) #перевод значений показателей в числовой

  list_1 <- colnames(imp[c(which((imp < exp)))]) #список названия тех отраслей промышленности, по которым экспорт превышает импорт
  list_2 <- colnames(imp[c(which((imp > exp)))]) #список названия тех отраслей промышленности, по которым импорт превышает экспорт
 
   return(list(list_2,list_1)) #замечение: при (list_2,list_1) все еще в получившемся списке первый элемент это list_1
}


# Применение


file_name <- "C:/Users/User/studyR/data/ExpImp.Rdata"
load(file_name)
a <- 'Иркутская область'
lists = get_lists_from_data(a)
print(a)
print("названия тех отраслей промышленности, по которым экспорт превышает импорт:")
lists[1]
print("названия тех отраслей промышленности, по которым импорт превышает экспорт:")
lists[2]

