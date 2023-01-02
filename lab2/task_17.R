# library(ggplot2)
# library(magrittr)
# library(readr)
# library(tidyr)
# library(data.table)
# library(repr)
# library(stringr)



library(dplyr)
library(tibble)


# Используйте файл outcome-of-care-measures.csv
# Напишите функцию, которая принимает на вход название штата 
# и выдает на выход список, который содержит количество больниц в штате 
# и максимальный и минимальный уровень смертности от каждого из трех заболеваний в больницах этого штата.

#Для каждого заболевания 

name = "AL"

file_name = "C:/Users/User/studyR/data/outcome-of-care-measures.csv"
input <- data.table::fread(file_name)
removeQuotes <- function(x) gsub("\"", "", x)
input <- input %>%
  mutate_if(is.character, removeQuotes)

state_table <- tibble(input)

state_table <- state_table[,(colnames(state_table) %in% c('\"State\"',
                                                          '\"Hospital 30-Day Death (Mortality) Rates from Heart Attack\"',
                                                          '\"Hospital 30-Day Death (Mortality) Rates from Pneumonia\"',
                                                          '\"Hospital 30-Day Death (Mortality) Rates from Heart Failure\"'))]
state_table <- state_table[(state_table[,1] == name), ]


state_table$`"Hospital 30-Day Death (Mortality) Rates from Heart Attack"` = as.numeric((as.character((state_table$`"Hospital 30-Day Death (Mortality) Rates from Heart Attack"`))))
state_table$`"Hospital 30-Day Death (Mortality) Rates from Heart Failure"` = as.numeric(as.character(state_table$`"Hospital 30-Day Death (Mortality) Rates from Heart Failure"`))
state_table$`"Hospital 30-Day Death (Mortality) Rates from Pneumonia"` = as.numeric(as.character(state_table$`"Hospital 30-Day Death (Mortality) Rates from Pneumonia"`))

#grep('.*Импорт', colnames(region))
max_rate_death <- c(max(na.omit(state_table[,2])), #heart attack
                    max(na.omit(state_table[,3])), #heart failure
                    max(na.omit(state_table[,4]))) #pneumonia

min_rate_death <- c(min(na.omit(state_table[,2])), #heart attack
                   min(na.omit(state_table[,3])), #heart failure
                   min(na.omit(state_table[,4]))) #pneumonia)

