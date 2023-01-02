#_trades.RData_ 

#Объедините таблицы в одну, 
#уберите столбец с территорией торговли, т.к. там только Евросоюз
#оставим только строки с экспортом и импортом, 
#вынесете данные по экспорту и импорту в отдельные переменные. 
#Постройте линейный график изменения экспорта по годам обозначив разные группы экспортируемых товаров разным цветом. 
#Подпишите значения на графике. 
#Отрегулируйте местоположение подписей так, чтобы рисунок был максимально наглядным. 
#Придумайте новые названия графика и осей.


library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

load("C:/Users/User/studyR/data/trades.RData")

df <- trades |>
  bind_rows() |> #Объедините таблицы в одну, 
  select(-geo) |> #уберите столбец с территорией торговли, т.к. там только Евросоюз
  dplyr::filter(str_detect(indic_et, "Exports in|Imports in")) |> #оставим только строки с экспортом и импортом, 
  pivot_wider(names_from = indic_et, values_from = values) |> 
  rename(
    export = "Exports in million of ECU/EURO",
    import = "Imports in million of ECU/EURO"
  )#вынесете данные по экспорту и импорту в отдельные переменные. 

#Постройте линейный график изменения экспорта по годам обозначив разные группы экспортируемых товаров разным цветом. 
#Подпишите значения на графике. 
#Отрегулируйте местоположение подписей так, чтобы рисунок был максимально наглядным. 
#Придумайте новые названия графика и осей.

rdf<- df[,c("time", "export" )]
rdf$name<-df$sitc06
rdf<- rdf %>% group_by(name, time) %>% summarise(export = sum(export))
g <- 
  ggplot(rdf, aes(x = time, y = export, group = name, color =name)) + 
  geom_line() + 
  geom_point() +
  geom_text(aes(label = export,x = time, y = export), size = 3,parse = TRUE) + 
  ggtitle("изменения экспорта по годам разных групп товаров" ) +
  xlab('год') + 
  ylab('Сумма экспорта') 

plotly::ggplotly(g)





