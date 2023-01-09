#Ивко Татьяна Витальевна - для региона 64 рассчитайте урожайность пшеницы 
#в 2012 году, взяв для расчета средние суммы активных температур за 
#предыдущие 5 лет, с 30 ближайших метеостанций
#Saratov #SARATOV

# подключим библиотеки
library(tidyverse)
library(lubridate)
library(dplyr)
library(rnoaa)


# скачиваем список всех метеостанций мира, процесс может занять до нескольких минут
station_data = ghcnd_stations()

#station_data = ghcnd_stations() #Может занять несколько минут лучше 
#выполнить один раз в месте с хорошим интернетом и сохранить результат
station_data = read.csv("station_data.csv")

# для региона 64 создаем таблицу с коррдинатами столицы 
saratov = data.frame(id = "SARATOV", latitude = 51.54056,  longitude = 46.00861)

#После получения всписка всех станций, получите список станций ближайших 
#к столице вашего региона,создав таблицу с именем региона и координатами 
#его столицы

saratov_around = meteo_nearby_stations(lat_lon_df = saratov, station_data = station_data,
                                    limit = 30, var = c("PRCP", "TAVG"),
                                    year_min = 2012, year_max = 2012)

# проверяем тип данных (получили list из одной строки с вложеной таблицей)
class(saratov_around)
# чтобы далее работать с данными надо перевести строку в формат таблицы, для этого просто обращаемся к этой строке с таблицей в текстовом формате
saratov_around = saratov_around[[1]]

# создаем вектор с id номерами выбранных метеостанций 
meteo_id_30 = saratov_around[,1]

# получаем все данные со всех выбранных метеостанций
all_meteo_data = meteo_tidy_ghcnd(stationid = meteo_id_30)

# Precipitation - осадки
all_meteo_data$prcp 
# Snow - высота зимних осадков
all_meteo_data$snwd 
# Temoerature air averaged - средняя темп воздуха за день
all_meteo_data$tavg 
# Temoerature air max - максимальная темп воздуха за день
all_meteo_data$tmax 
# Temoerature air min - минимальная темп воздуха за день
all_meteo_data$tmin 

# добавляем в таблицу колонки с разделенной датой (год, месяц, день)
all_meteo_data = all_meteo_data %>% mutate(year = year(date), month = month(date), day =yday(date))

# фильтруем по годам так, чтобы выбрать период 5 лет, предшествующий 2012 году
all_meteo_data = all_meteo_data %>% filter(year >  2006 & year < 2011)

# переводим размерности значений в привычную для пользователей форму (меторологи не любят точки)
all_meteo_data = all_meteo_data %>% mutate(prcp = prcp /10, snwd = snwd /10, tavg = tavg /10, tmax =tmax/10, tmin = tmin/10)

# в векторе можно подменить все значения меньше 5 на 0 (делаем это через substitute)
all_meteo_data$tavg[all_meteo_data$tavg < 10] = 0

# группируем по месяцам, годам и id 
# и сводим в таблицу с помесячными суммами активных температур на все станции и годы. сумма активных температур меньше 30
sum_monht_tavg = all_meteo_data %>% group_by(month, id, year) %>% 
  summarise(sum_tavg = sum(tavg, na.rm = TRUE)) %>% print(n=500)

# сбрасываем группировку
sum_monht_tavg = ungroup(sum_monht_tavg)

# группируем по месяцам и сводим в таблицу со средними помесячными активными температурами 
mean_month_tavg = sum_monht_tavg %>% group_by(month) %>%
  summarise(mean_tavg = mean(sum_tavg, na.rm=TRUE))

# считаем среднюю сумму активных температур (>10) за год
sum_year_tavg = sum(mean_month_tavg$mean_tavg) 
#получили 3023.7797 градусов

# добавляем в сводную таблицу переменные из табл 1 методички https://ecologymodeling.github.io/Tests.html
mean_month_tavg = mean_month_tavg %>% mutate(
  afi = c(0,0,0,32.11,26.31,25.64,23.2,18.73,16.3,13.83,0,0),
  bfi = c(0,0,0,11.3,9.26,9.03,8.16,6.59,5.73,4.87,0,0),
  di = c(0,0,0,0.33,1,1,1,0.32,0,0,0,0))

# добавляем в сводную таблицу переменную Fi, расчитанную по формуле из методички
mean_month_tavg = mean_month_tavg %>% mutate(Fi = afi+bfi*mean_tavg)

# рассчитываем урожайность пшеницы по формуле из методички с учетом заданных констант
Yj = 10^5 * (sum(mean_month_tavg$Fi * mean_month_tavg$di * 300 / (
  1600 * 2.2 * (100 - 25))))
Yj = Yj / 1000 / 100
# В итоге, урожайность пшеницы в регионе 64 в 2012 году 
# составит = 19.3804 ц/га
