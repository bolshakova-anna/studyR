#_ExpImp.Rdf_ 
#Приведите данные к формату tidydf
#Отобразите суммарные значения экспорта и импорта по субъектам федерации находящимся в Центральном федеральном округе
#в виде столбчатой диаграммы [сгруппировав значения по названию субъекта федерации]. 
#Экспорт и импорт обозначьте разными цветами.
#Сделайте второй график, наложив столбцы экспорта и импорта один на другой и подписав значения разниц между экспортом и ипортом


library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(plotly)

path <- "C:/Users/User/studyR/data/ExpImp.Rdata"
load(path)
region <- "Центральный федеральный округ"

plot_first <- function(region){
  df <- ExpImp
  df <- df[complete.cases(df),]
  for (i in 2:length(names(df))) {
    df[[i]] <- gsub("-", 0, df[[i]])
    df[[i]] <- as.numeric(df[[i]])
  }
  flt <- str_detect(df$Регион, 'федеральный округ')
  rdf <- mutate(df, Округ = if_else(flt, Регион, NULL))
  rdf <- fill(rdf, Округ)
  flt2 <- !str_detect(rdf$Регион, 'Федерация|федеральный округ')
  rdf <- filter(rdf, flt2)
  match_exp <- select_at(rdf, vars(matches("Экспорт")))
  match_imp <- select_at(rdf, vars(matches("Импорт")))
  match_exp$Сумма <- rowSums(match_exp, na.rm = TRUE)
  match_imp$Сумма <- rowSums(match_imp, na.rm = TRUE)
  rdf$СуммаЭкспорта <- match_exp$Сумма
  rdf$СуммаИмпорта <- match_imp$Сумма
  rdf <- filter(rdf, Округ == region)
  rdf <- rdf[,c("Регион", "СуммаЭкспорта", "СуммаИмпорта" )]
  rdf <- pivot_longer(rdf, !Регион, names_to = "Метрика", values_to = "млн долларов США")
  sum_reg <- rdf %>% group_by(Регион, `Метрика`) 
  sum_reg <- sum_reg %>% summarise(sum = sum(`млн долларов США`))
  sum_reg |>
    ggplot(mapping = aes(x = Регион, y = sum, fill = `Метрика`)) +
    geom_col(color = 'black', size = 0.2, position = 'dodge') + 
    ggtitle(region) + ylab('млн долларов США') + 
    coord_flip() 
  
}

plot_second <- function(region){
  df <- ExpImp
  df <- df[complete.cases(df),]
  for (i in 2:length(names(df))) {
    df[[i]] <- gsub("-", 0, df[[i]])
    df[[i]] <- as.numeric(df[[i]])
  }
  flt <- str_detect(df$Регион, 'федеральный округ')
  rdf <- mutate(df, Округ = if_else(flt, Регион, NULL))
  rdf <- fill(rdf, Округ)
  flt2 <- !str_detect(rdf$Регион, 'Федерация|федеральный округ')
  rdf <- filter(rdf, flt2)
  match_exp <- select_at(rdf, vars(matches("Экспорт")))
  match_imp <- select_at(rdf, vars(matches("Импорт")))
  match_exp$Сумма <- rowSums(match_exp, na.rm = TRUE)
  match_imp$Сумма <- rowSums(match_imp, na.rm = TRUE)
  rdf$СуммаЭкспорта <- match_exp$Сумма
  rdf$СуммаИмпорта <- match_imp$Сумма
  rdf$Разница <- abs(match_exp$Сумма - match_imp$Сумма)
  rdf <- filter(rdf, Округ == region)
  g <- ggplot(rdf) +
    geom_col(aes(x = rdf$`Регион`, y = rdf$`СуммаЭкспорта`),fill = "blue", width = 0.2) + 
    geom_col(aes(x = rdf$`Регион`, y = rdf$`СуммаИмпорта`), alpha = 0.3, fill = "red", width = 0.6) +
    geom_text(aes(label = rdf$`Разница`, x = rdf$Регион, y = max(rdf$СуммаЭкспорта,rdf$СуммаИмпорта))) + 
    ggtitle(region) + 
    xlab('Регион') +
    ylab('млн долларов США') + 
    coord_flip() 
  plotly::ggplotly(g)
}

plot_first(region)

plot_second(region)

